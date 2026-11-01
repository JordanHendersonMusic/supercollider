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

#pragma once

#include <QElapsedTimer>
#include <QHash>
#include <QJsonObject>
#include <QJsonValue>
#include <QList>
#include <QObject>
#include <QPointer>
#include <QProcess>
#include <QSet>
#include <QTimer>

#include <functional>

class QTextDocument;

namespace ScIDE {

class Document;
class DocumentManager;
namespace Settings {
class Manager;
}

// A client of sc_lsp, the language server in langutils/sc_lsp. It keeps the server's documents in step with the IDE's,
// and holds the diagnostics it publishes. Positions are UTF-16, as QTextDocument's are.
class LspClient : public QObject {
    Q_OBJECT

public:
    struct Diagnostic {
        int start; // character positions in the document
        int end;
        int severity; // 1 error, 2 warning, 3 information
        QString message;
    };
    struct Location {
        QString path; // empty for a document without a file, see document
        QPointer<Document> document;
        int line;
        int character;
        int endLine;
        int endCharacter;
    };
    // A top-level statement of an scd document, with any brackets around it, as sc_lsp's sclang/regions.
    struct Region {
        int start; // character positions in the document
        int end;
    };
    using Callback = std::function<void(const QJsonValue& result)>;
    // Called with the regions of the document's current text, or with ok false when they couldn't be had.
    using RegionsCallback = std::function<void(const QList<Region>& regions, bool ok)>;

    LspClient(DocumentManager*, Settings::Manager*, QObject* parent);
    ~LspClient();

    bool isReady() const { return mReady; }
    // Starts, or restarts, sc_lsp, if enabled in the settings.
    void start();
    void stop();

    // Sends a request about the document at a character position. The document's latest text is sent first.
    void requestAt(const QString& method, Document*, int position, Callback, QJsonObject extra = {});
    QList<Diagnostic> diagnostics(Document*) const;

    // Whether the server is running and gives regions.
    bool hasRegions() const { return mReady && mHasRegions; }
    // The last regions the server published, which can be a little older than the text, e.g., for drawing.
    QList<Region> regions(Document*) const;
    // Whether regions() are of the document's current text.
    bool regionsCurrent(Document*) const;
    // The regions of the document's current text: straight away when regions() are current, otherwise once the
    // server answers. Fails if the text changes before then, or there is no server.
    void currentRegions(Document*, RegionsCallback);

    // Turns a location in a result into one the IDE can open.
    Location location(const QJsonValue&) const;
    // Opens a location, as go to definition does.
    static void open(const Location&);
    // The character position of an LSP position in a document.
    static int position(QTextDocument*, const QJsonValue& lspPosition);

signals:
    void diagnosticsChanged(Document*);
    void regionsChanged(Document*);
    void readyChanged(bool);

public slots:
    void applySettings(Settings::Manager*);

private slots:
    void onReadyRead();
    void onFinished(int exitCode, QProcess::ExitStatus);
    void onDocumentOpened(Document*);
    void onDocumentClosed(Document*);
    void onDocumentSaved(Document*);
    void onContentsChanged();
    void flushChanges();

private:
    struct Tracked {
        QString uri;
        int version = 0;
        bool dirty = false;
        QList<Region> regions;
        int regionsVersion = -1; // the version regions are of
    };

    QString serverPath() const;
    QStringList serverArguments() const;
    QJsonObject initializationOptions() const;
    void send(const QJsonObject&);
    void notify(const QString& method, const QJsonObject& params);
    void request(const QString& method, const QJsonObject& params, Callback);
    void handle(const QJsonObject&);
    void openDocument(Document*);
    void closeDocument(Document*);
    void sendChange(Document*);
    QString uriFor(Document*) const;
    Document* documentFor(const QString& uri) const;
    void post(const QString&);
    QList<Region> toRegions(Document*, const QJsonValue& ranges) const;
    void setRegions(Document*, const QJsonObject& published);

    DocumentManager* mDocManager;
    Settings::Manager* mSettings;
    QProcess* mProcess = nullptr;
    QByteArray mBuffer;
    int mNextId = 0;
    QHash<int, Callback> mPending;
    bool mReady = false;
    bool mStopping = false;
    bool mEnabled = true;
    bool mHasRegions = false;
    QHash<Document*, Tracked> mDocuments;
    QHash<QString, QList<Diagnostic>> mDiagnostics; // by uri
    QTimer mChangeTimer;
    QElapsedTimer mSinceStart;
    int mRestarts = 0;
    bool mWarnedMissing = false;
};

} // namespace ScIDE
