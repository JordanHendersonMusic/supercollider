/*
    SuperCollider Qt IDE
    Copyright (c) 2026 SuperCollider Team
    https://supercollider.github.io/

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
*/

#include "lsp_client.hpp"

#include "doc_manager.hpp"
#include "main.hpp"
#include "settings/manager.hpp"
#include "util/standard_dirs.hpp"

#include <QCoreApplication>
#include <QDir>
#include <QFileInfo>
#include <QJsonArray>
#include <QJsonDocument>
#include <QStandardPaths>
#include <QTextBlock>
#include <QTextDocument>
#include <QUrl>

#include <yaml-cpp/yaml.h>

#include <fstream>

namespace ScIDE {

namespace {
constexpr int maxRestarts = 5;
constexpr int changeDelayMs = 150;
}

LspClient::LspClient(DocumentManager* docManager, Settings::Manager* settings, QObject* parent):
    QObject(parent),
    mDocManager(docManager),
    mSettings(settings) {
    mChangeTimer.setSingleShot(true);
    mChangeTimer.setInterval(changeDelayMs);
    connect(&mChangeTimer, &QTimer::timeout, this, &LspClient::flushChanges);
    connect(docManager, &DocumentManager::opened, this, [this](Document* d, int, int) { onDocumentOpened(d); });
    connect(docManager, &DocumentManager::closed, this, &LspClient::onDocumentClosed);
    connect(docManager, &DocumentManager::saved, this, &LspClient::onDocumentSaved);
    mEnabled = settings->value("IDE/lsp/enabled").toBool();
}

LspClient::~LspClient() { stop(); }

void LspClient::applySettings(Settings::Manager* settings) {
    const bool enabled = settings->value("IDE/lsp/enabled").toBool();
    if (enabled == mEnabled)
        return;
    mEnabled = enabled;
    if (enabled)
        start();
    else
        stop();
}

////////////////////////////////////////////////////////////////////////////////
// The server process
////////////////////////////////////////////////////////////////////////////////

QString LspClient::serverPath() const {
    const QString configured = mSettings->value("IDE/lsp/path").toString();
    if (!configured.isEmpty())
        return configured;
#ifdef Q_OS_WIN
    const QString name = "sc_lsp.exe";
#else
    const QString name = "sc_lsp";
#endif
    const QString appDir = QCoreApplication::applicationDirPath();
    // Installed beside the IDE, in a macOS bundle, or in a build tree.
    for (const QString& candidate :
         { appDir + "/" + name, appDir + "/../MacOS/" + name, appDir + "/../../langutils/sc_lsp/" + name,
           appDir + "/../../../langutils/sc_lsp/" + name }) {
        QFileInfo info(candidate);
        if (info.exists() && info.isExecutable())
            return info.canonicalFilePath();
    }
    return QStandardPaths::findExecutable(name);
}

// The class library sclang compiles: its config's include paths, the defaults unless it excludes them.
QJsonObject LspClient::initializationOptions() const {
    QString configFile = mSettings->value("IDE/interpreter/configFile").toString();
    if (configFile.isEmpty())
        configFile = standardDirectory(ScConfigUserDir) + "/sclang_conf.yaml";

    QJsonArray include, exclude;
    bool excludeDefaults = false;
    QString interpretedVersion;
    if (QFileInfo::exists(configFile)) {
        try {
            std::ifstream in(configFile.toStdString());
            const YAML::Node doc = YAML::Load(in);
            const auto paths = [&](const char* key, QJsonArray& out) {
                const YAML::Node node = doc[key];
                if (node && node.IsSequence())
                    for (const auto& p : node)
                        if (p.IsScalar() && !p.as<std::string>().empty())
                            out.append(QString::fromStdString(p.as<std::string>()));
            };
            paths("includePaths", include);
            paths("excludePaths", exclude);
            if (const YAML::Node node = doc["excludeDefaultPaths"])
                excludeDefaults = node.as<bool>();
            if (const YAML::Node node = doc["interpretedVersion"])
                interpretedVersion = QString::fromStdString(node.as<std::string>());
        } catch (std::exception&) {}
    }
    QJsonArray library;
    if (!excludeDefaults) {
        library.append(standardDirectory(ScResourceDir) + "/SCClassLibrary");
        library.append(standardDirectory(ScExtensionSystemDir));
        library.append(standardDirectory(ScExtensionUserDir));
    }
    for (const auto& p : include)
        library.append(p);

    QJsonObject options;
    options["libraryPaths"] = library;
    options["excludePaths"] = exclude;
    if (!interpretedVersion.isEmpty())
        options["interpretedVersion"] = interpretedVersion;
    const int port = mSettings->value("IDE/interpreter/sclangPort").toInt();
    options["sclangPort"] = port > 0 ? port : 57120;
    return options;
}

void LspClient::start() {
    if (!mEnabled)
        return;
    stop();
    const QString path = serverPath();
    if (path.isEmpty()) {
        if (!mWarnedMissing)
            post(tr("The language server, sc_lsp, wasn't found. Set its path in IDE/lsp/path to use it.\n"));
        mWarnedMissing = true;
        return;
    }
    mStopping = false;
    mBuffer.clear();
    mPending.clear();
    mDiagnostics.clear();
    mProcess = new QProcess(this);
    mProcess->setProcessChannelMode(QProcess::SeparateChannels);
    // Its log isn't needed, and an unread pipe could fill.
    mProcess->setStandardErrorFile(QProcess::nullDevice());
    connect(mProcess, &QProcess::readyReadStandardOutput, this, &LspClient::onReadyRead);
    connect(mProcess, QOverload<int, QProcess::ExitStatus>::of(&QProcess::finished), this, &LspClient::onFinished);
    mProcess->start(path, QStringList());
    if (!mProcess->waitForStarted(5000)) {
        post(tr("The language server, %1, couldn't be started.\n").arg(path));
        mProcess->deleteLater();
        mProcess = nullptr;
        return;
    }
    mSinceStart.start();

    QJsonObject general;
    general["positionEncodings"] = QJsonArray { "utf-16" };
    QJsonObject capabilities;
    capabilities["general"] = general;
    QJsonObject params;
    params["processId"] = static_cast<qint64>(QCoreApplication::applicationPid());
    params["clientInfo"] = QJsonObject { { "name", "scide" } };
    params["capabilities"] = capabilities;
    params["initializationOptions"] = initializationOptions();
    request("initialize", params, [this](const QJsonValue&) {
        notify("initialized", QJsonObject());
        mReady = true;
        mDocuments.clear();
        for (Document* doc : mDocManager->documents())
            openDocument(doc);
        emit readyChanged(true);
    });
}

void LspClient::stop() {
    if (!mProcess)
        return;
    mStopping = true;
    if (mReady) {
        request("shutdown", QJsonObject(), nullptr);
        notify("exit", QJsonObject());
        mProcess->waitForFinished(1000);
    }
    if (mProcess->state() != QProcess::NotRunning) {
        mProcess->kill();
        mProcess->waitForFinished(1000);
    }
    mProcess->deleteLater();
    mProcess = nullptr;
    const bool wasReady = mReady;
    mReady = false;
    mDocuments.clear();
    mDiagnostics.clear();
    for (Document* doc : mDocManager->documents())
        emit diagnosticsChanged(doc);
    if (wasReady)
        emit readyChanged(false);
}

void LspClient::onFinished(int exitCode, QProcess::ExitStatus status) {
    if (mStopping || !mProcess)
        return;
    mReady = false;
    mProcess->deleteLater();
    mProcess = nullptr;
    emit readyChanged(false);
    // A crash shouldn't lose the editor's features for good, but a server that keeps crashing is left stopped.
    if (mSinceStart.elapsed() > 120000)
        mRestarts = 0;
    if (++mRestarts > maxRestarts) {
        post(tr("The language server stopped %1 times, it won't be restarted.\n").arg(maxRestarts));
        return;
    }
    post(tr("The language server stopped (%1), restarting it.\n")
             .arg(status == QProcess::CrashExit ? tr("crashed") : tr("exit code %1").arg(exitCode)));
    QTimer::singleShot(1000, this, &LspClient::start);
}

void LspClient::post(const QString& text) {
    QString line = "LSP: " + text;
    Main::scProcess()->post(line);
}

////////////////////////////////////////////////////////////////////////////////
// Messages
////////////////////////////////////////////////////////////////////////////////

void LspClient::send(const QJsonObject& message) {
    if (!mProcess)
        return;
    const QByteArray body = QJsonDocument(message).toJson(QJsonDocument::Compact);
    mProcess->write("Content-Length: " + QByteArray::number(body.size()) + "\r\n\r\n" + body);
}

void LspClient::notify(const QString& method, const QJsonObject& params) {
    send(QJsonObject { { "jsonrpc", "2.0" }, { "method", method }, { "params", params } });
}

void LspClient::request(const QString& method, const QJsonObject& params, Callback callback) {
    const int id = ++mNextId;
    if (callback)
        mPending.insert(id, std::move(callback));
    send(QJsonObject { { "jsonrpc", "2.0" }, { "id", id }, { "method", method }, { "params", params } });
}

void LspClient::onReadyRead() {
    if (!mProcess)
        return;
    mBuffer += mProcess->readAllStandardOutput();
    while (true) {
        const int headerEnd = mBuffer.indexOf("\r\n\r\n");
        if (headerEnd < 0)
            return;
        int length = -1;
        for (const QByteArray& line : mBuffer.left(headerEnd).split('\n')) {
            const QByteArray trimmed = line.trimmed();
            if (trimmed.toLower().startsWith("content-length:"))
                length = trimmed.mid(15).trimmed().toInt();
        }
        if (length < 0) {
            mBuffer.remove(0, headerEnd + 4);
            continue;
        }
        if (mBuffer.size() < headerEnd + 4 + length)
            return;
        const QByteArray body = mBuffer.mid(headerEnd + 4, length);
        mBuffer.remove(0, headerEnd + 4 + length);
        const QJsonDocument json = QJsonDocument::fromJson(body);
        if (json.isObject())
            handle(json.object());
    }
}

void LspClient::handle(const QJsonObject& message) {
    if (message.contains("id") && !message.contains("method")) {
        const auto callback = mPending.take(message["id"].toInt());
        if (callback)
            callback(message.contains("error") ? QJsonValue() : message["result"]);
        return;
    }
    const QString method = message["method"].toString();
    if (method == "textDocument/publishDiagnostics") {
        const QJsonObject params = message["params"].toObject();
        const QString uri = params["uri"].toString();
        Document* doc = documentFor(uri);
        QList<Diagnostic> list;
        if (doc) {
            for (const auto& value : params["diagnostics"].toArray()) {
                const QJsonObject d = value.toObject();
                const QJsonObject range = d["range"].toObject();
                list.append({ position(doc->textDocument(), range["start"]),
                              position(doc->textDocument(), range["end"]), d["severity"].toInt(1),
                              d["message"].toString() });
            }
        }
        mDiagnostics[uri] = list;
        if (doc)
            emit diagnosticsChanged(doc);
    }
}

////////////////////////////////////////////////////////////////////////////////
// Documents
////////////////////////////////////////////////////////////////////////////////

QString LspClient::uriFor(Document* doc) const {
    if (!doc->filePath().isEmpty())
        return QUrl::fromLocalFile(doc->filePath()).toString(QUrl::FullyEncoded);
    return "untitled:" + QString::fromLatin1(doc->id());
}

Document* LspClient::documentFor(const QString& uri) const {
    for (auto it = mDocuments.begin(); it != mDocuments.end(); ++it)
        if (it->uri == uri)
            return it.key();
    return nullptr;
}

void LspClient::openDocument(Document* doc) {
    if (!mReady || mDocuments.contains(doc) || doc->isPlainText())
        return;
    Tracked tracked;
    tracked.uri = uriFor(doc);
    tracked.version = 1;
    mDocuments.insert(doc, tracked);
    connect(doc->textDocument(), &QTextDocument::contentsChanged, this, &LspClient::onContentsChanged,
            Qt::UniqueConnection);
    QJsonObject item;
    item["uri"] = tracked.uri;
    item["languageId"] = "supercollider";
    item["version"] = tracked.version;
    item["text"] = doc->textDocument()->toPlainText();
    notify("textDocument/didOpen", QJsonObject { { "textDocument", item } });
}

void LspClient::closeDocument(Document* doc) {
    const auto it = mDocuments.find(doc);
    if (it == mDocuments.end())
        return;
    const QString uri = it->uri;
    mDocuments.erase(it);
    mDiagnostics.remove(uri);
    disconnect(doc->textDocument(), &QTextDocument::contentsChanged, this, &LspClient::onContentsChanged);
    notify("textDocument/didClose", QJsonObject { { "textDocument", QJsonObject { { "uri", uri } } } });
}

void LspClient::onDocumentOpened(Document* doc) { openDocument(doc); }

void LspClient::onDocumentClosed(Document* doc) { closeDocument(doc); }

void LspClient::onDocumentSaved(Document* doc) {
    const auto it = mDocuments.find(doc);
    if (it == mDocuments.end()) {
        openDocument(doc);
        return;
    }
    // Saved under a new name, e.g., an untitled document, which may now be a class file.
    if (it->uri != uriFor(doc)) {
        closeDocument(doc);
        openDocument(doc);
        emit diagnosticsChanged(doc);
        return;
    }
    sendChange(doc);
    notify("textDocument/didSave", QJsonObject { { "textDocument", QJsonObject { { "uri", it->uri } } } });
}

void LspClient::onContentsChanged() {
    auto* textDoc = qobject_cast<QTextDocument*>(sender());
    for (auto it = mDocuments.begin(); it != mDocuments.end(); ++it)
        if (it.key()->textDocument() == textDoc)
            it->dirty = true;
    mChangeTimer.start();
}

void LspClient::flushChanges() {
    for (auto it = mDocuments.begin(); it != mDocuments.end(); ++it)
        if (it->dirty)
            sendChange(it.key());
}

void LspClient::sendChange(Document* doc) {
    const auto it = mDocuments.find(doc);
    if (it == mDocuments.end() || !it->dirty)
        return;
    it->dirty = false;
    ++it->version;
    QJsonObject item { { "uri", it->uri }, { "version", it->version } };
    QJsonArray changes { QJsonObject { { "text", doc->textDocument()->toPlainText() } } };
    notify("textDocument/didChange", QJsonObject { { "textDocument", item }, { "contentChanges", changes } });
}

QList<LspClient::Diagnostic> LspClient::diagnostics(Document* doc) const {
    const auto it = mDocuments.find(doc);
    if (it == mDocuments.end())
        return {};
    return mDiagnostics.value(it->uri);
}

////////////////////////////////////////////////////////////////////////////////
// Requests and positions
////////////////////////////////////////////////////////////////////////////////

void LspClient::requestAt(const QString& method, Document* doc, int pos, Callback callback, QJsonObject extra) {
    if (!mReady || !mDocuments.contains(doc)) {
        callback(QJsonValue());
        return;
    }
    sendChange(doc);
    const QTextBlock block = doc->textDocument()->findBlock(pos);
    // A block is a line, as QPlainTextEdit splits lines, and positions in it are UTF-16, as LSP's are.
    extra["textDocument"] = QJsonObject { { "uri", mDocuments[doc].uri } };
    extra["position"] = QJsonObject { { "line", block.blockNumber() }, { "character", pos - block.position() } };
    request(method, extra, std::move(callback));
}

int LspClient::position(QTextDocument* doc, const QJsonValue& value) {
    const QJsonObject p = value.toObject();
    const QTextBlock block = doc->findBlockByNumber(p["line"].toInt());
    if (!block.isValid())
        return doc->characterCount() - 1;
    return block.position() + qMin(p["character"].toInt(), block.length() - 1);
}

LspClient::Location LspClient::location(const QJsonValue& value) const {
    const QJsonObject l = value.toObject();
    const QString uri = l["uri"].toString();
    const QJsonObject range = l["range"].toObject();
    Location out;
    out.document = documentFor(uri);
    const QUrl url(uri);
    if (url.isLocalFile())
        out.path = url.toLocalFile();
    out.line = range["start"].toObject()["line"].toInt();
    out.character = range["start"].toObject()["character"].toInt();
    out.endLine = range["end"].toObject()["line"].toInt();
    out.endCharacter = range["end"].toObject()["character"].toInt();
    return out;
}

void LspClient::open(const Location& l) {
    DocumentManager* manager = Main::documentManager();
    Document* doc = l.document ? l.document.data() : nullptr;
    if (!doc && !l.path.isEmpty())
        doc = manager->open(l.path, -1, 0, false);
    if (!doc)
        return;
    QTextDocument* text = doc->textDocument();
    const int start = position(text, QJsonObject { { "line", l.line }, { "character", l.character } });
    const int end = position(text, QJsonObject { { "line", l.endLine }, { "character", l.endCharacter } });
    emit manager->showRequest(doc, start, qMax(0, end - start));
}

} // namespace ScIDE
