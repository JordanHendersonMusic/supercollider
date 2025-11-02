// The Object class repesents an interface to a *complete* object.
// Alterantively, AbstractObject is an incomplete object.
// There are many protocols that Object must follow, these have been added over the years and have grown too large.
// One should carefully consider if it is possible to restructure the code before adding any new methods here as it is a requirement on every single object that will ever be made.

// These messages are broken up here into tiers to make it a little more obvious why these messages need to exist.
// They are only grouped into protocols when they don't span tiers.
// The presence of a protocol that spans a tier is a red flag and indicates an unusual and 'clever' relationship between the interpreter and this class.
Object : AbstractObject {
	classvar <dependantsDictionary, currentEnvironment, topEnvironment, <uniqueMethods;

	// Constants in Object are keywords and accessible from any scope.
	// TODO: should this be removed?
	const nl = "\n";

	///////////// VM METHODS
	// Top level messages, if these are not defined the interpeter will enact undefined behaviour.

	// Note, mustBeBoolean must throw as the jump bytecode instructions currently cannot skip the instructions for the true and false blocks.
	mustBeBoolean { MustBeBooleanError(nil, this).throw }
	// Interpreter's message bytecodes assume this exists and will segfault and call another undetermined method if this isn't defined.
	doesNotUnderstand { |selector ...args, kwargs| ^DoesNotUnderstandError(this, selector, args, kwargs).throw }

	// Cannot override the concept of identity, it is tied to the implementation of PyrSlot and PyrObject.
	identityHash { _ObjectHash; ^this.primitiveFailed }
	=== { arg obj; _Identical; ^this.primitiveFailed }
	!== { arg obj;_NotIdentical; ^this.primitiveFailed }




	///////////// CORE INTERPRETER METHODS
	// These methods are needed for the core interpreter to work.
	// Exactly what we might consider 'core' and not is some what arbitary, these tiers aren't exact, just guidelines.

	// Printing into the post window
	asStringForPostWindow { ^this.asString }

	// Printing for errors. These messages CANNOT throw.
	// If an Object's asString method can throw, the following should be overloaded to a safe implementation.
	asStringForError { ^this.asString }
	asStringForErrorPostln { ^this.dump }

	// Message dispatch methods.
	// The interpreter transforms some syntax shortcuts into these messsages.
	// These methods are renamed from AbstractObject, but are copied here to avoid the performance hit. Ensure they stay in sync!
	performArgs { |selector, args, kwargs| _ObjectPerformArgs;	^this.primitiveFailed}
	superPerformArgs { |selector, args, kwargs| _ObjectSuperPerformArgs; ^this.primitiveFailed  }
	perform { | selector ... args| _ObjectPerform; ^this.primitiveFailed }
	performMsg { |msg| _ObjectPerformMsg; ^this.primitiveFailed }
	// This method is called with the follow syntax `f.foo(*args)`.
	// If it differs from the below defintion, you will get different behaviour depending on whether you the array expand syntax.
	performList { | ...args, kwargs| _ObjectPerformList; ^this.primitiveFailed }
	// super.perform(selector, arg) doesn't do what you might think.
	// \perform would be looked up in the superclass, not the selector you are interested in.
	// Hence these methods, which look up the selector in the superclass.
	// These methods must be called with this as the receiver.
	superPerform { | ... args, kwargs| _SuperPerform; ^this.primitiveFailed	}
	superPerformList { | ...args, kwargs| _SuperPerformList; ^this.primitiveFailed }

	// Being nil is tied to identity and the compiler inlines these methods.
	// You cannot reliably overload them.
	? { arg obj; ^this }
	?? { arg obj; ^this }
	!? { arg obj; ^obj.value(this) }
	isNil { ^false }
	notNil { ^true }

	// Used to tell the interpreter that the result returned from a function was not exception.
	isException { ^false }




	//////////// OTHER INTERPRETER METHODS
	// These methods are used for a variety of other functionality.
	// If these weren't implemented you would just get a DoesNotUnderstand error, rather than something nefarious.
	// Still, this tier tries to concern itself with key features of the language.

	// Coroutine support
	yield { _RoutineYield ^this.primitiveFailed }
	alwaysYield { _RoutineAlwaysYield ^super.sc_abstract_object_always_yield }
	yieldAndReset { |reset = true| _RoutineYieldAndReset ^super.sc_abstract_object_yield_and_reset(reset) }

	// Class membership
	class { _ObjectClass; ^this.primitiveFailed }
	isKindOf {  |aClass| ^this.sc_abstract_object_is_kind_of(aClass) }
	isMemberOf { arg aClass; _ObjectIsMemberOf; ^this.primitiveFailed }
	// Note, respondsTo should be avoided, it is bad OOP design as it doesn't allow `doesNotUndertstand` to come into effect.
	respondsTo { arg aSymbol; _ObjectRespondsTo; ^this.primitiveFailed }

	// Equality
	hash { _ObjectHash; ^this.primitiveFailed  }
	basicHash {  _ObjectHash; ^this.primitiveFailed  }
	== { |obj| ^this === obj }
	!= { | obj| ^not(this == obj) }

	// Copying, these are slightly different from AbstractObject!
	copy { ^this.shallowCopy }
	contentsCopy { ^this.shallowCopy } // This method has no documentation, what should it do?
	shallowCopy { _ObjectShallowCopy; ^this.primitiveFailed }
	copyImmutable { _ObjectCopyImmutable; ^this.primitiveFailed } // If object is immutable then return a shallow copy, else return receiver.
	deepCopy { _ObjectDeepCopy; ^this.primitiveFailed }





	///////////// FIRST TIER METHODS
	// Less important methods, still needed, but most features will still work.

	// Printing
	dump { _ObjectDump ^this.primitiveFailed }
	post { this.asString.post }
	postln { this.asString.postln; }
	postc { this.asString.postc }
	postcln { this.asString.postcln; }
	postcs { this.asCompileString.postln }

	value { ^this }
	valueArray { ^this }
	valueEnvir { ^this }
	valueArrayEnvir { ^this }
	valueArgs { | args, kwargs|	^this.performArgs(\value, args, kwargs) }

	while { |body|
		// compiler magic: the compiler inlines the following loop
		// thus an uninlinable while can be implemented using while itself
		while{ this.value } { body.value }
	}


	///////////// SECOND TIER METHODS
	// Everything else. There are some other tiers below.


	// Collection of throwing helpers.
	subclassResponsibility { |method| SubclassResponsibilityError(this, method, this.class).throw }
	shouldNotImplement { |method| ShouldNotImplementError(this, method, this.class).throw }
	outOfContextReturn { |method, result| OutOfContextReturnError(this, method, result).throw }
	immutableError { |value| ImmutableError(this, value).throw }
	deprecated { |method, alternateMethod| DeprecatedError(this, method, alternateMethod, this.class).throw }
	notYetImplemented { NotYetImplementedError(nil, this).throw }
	primitiveFailed { PrimitiveFailedError(this).throw	}


	// Error methods.
	throw {
		if (Error.handling) {
			error("throw during error handling!\n");
			this.asStringForErrorPostln;
			^this
		};
		thisThread.handleError(this);
	}
	reportError {
		error(this.asString);
		this.dumpBackTrace;
	}
	halt {
		thisProcess.nowExecutingPath = nil;
		OnError.run;
		this.prHalt
	}
	// _Halt will exit the interpreter, but isn't by itself an error, see Integer.exit.
	// Object.halt does trigger OnError.
	prHalt {
		_Halt
		^this.primitiveFailed
	}

	// Coroutine idle. Requires 'value'
	idle { |val|
		var time = thisThread.beats;
		while { thisThread.beats - time < val } { this.value.yield }
	}

	// Printing, but requires value.
	poll { ^this.value }

	// Perform only if Function. see Function-functionPerformList
	functionPerformList { | ...args, kwargs| ^this }

	// Requires equality.
	switch { |... cases|
		cases.pairsDo { | test, trueFunc |
			if (this == test.value) { ^trueFunc.value };
		};
		if (cases.size.odd) { ^cases.last.value };
		^nil
	}

	// Different types of equality.
	// Lazy equality: same as == for objects
	// "composed" for lazy operands (patterns, UGens)
	|==| { |that| ^that.prReverseLazyEquals(this) }
	|!=| { |that| ^not(this |==| that) }
	// a user might write `something |==| aPattern`
	// so we need to support reverse dispatch
	prReverseLazyEquals { |that| ^(this == that) }
	fuzzyEqual { arg that, precision=1.0; ^max(0.0, 1.0 - (abs(this - that)/precision)) }
	// TODO: This methods is broken. Do not use it.
	equals { arg that, properties;
		^that.respondsTo(properties) and: {
			properties.every { |selector| this.perform(selector) == that.perform(selector) }
		}
	}

	// TODO: why does this exist? How does it differ from 'class'?
	species { ^this.class }


	// Remainder of the is* methods.
	// These are code smells as they break polymorphism.
	// Try to avoid using them.
	isRest { ^false }
	isNumber { ^false }
	isInteger { ^false }
	isFloat { ^false }
	isSequenceableCollection { ^false }
	isCollection { ^false }
	isArray { ^false }
	isString { ^false }
	isPlaying { ^false }
	isFunction { ^false }
	isValidUGenInput { ^false }
	isUGen { ^false }
	isInputUGen { ^false }
	isOutputUGen { ^false }
	isControlUGen { ^false }

	// as* methods
	as { |aSimilarClass| ^aSimilarClass.newFrom(this) }
	asStream { ^this }
	asCollection { ^[this] }
	asSymbol { ^this.asString.asSymbol }
	asUGenInput { ^this }
	asControlInput { ^this }
	asAudioRateInput { ^if(this.rate != \audio) { K2A.ar(this) } { this } }
	asArray { ^this.asCollection.asArray }
	asRef { ^Ref.new(this) }
	asSequenceableCollection { ^this.asArray }
	asArchive {	^this.asTextArchive }

	// String conversions
	asString { |limit = 512|
		var string;
		_ObjectString
		string = String.streamContentsLimit({ arg stream; this.printOn(stream); }, limit);
		if (string.size >= limit, { ^(string ++ "...etc..."); });
		^string
	}
	asCompileString {
		_ObjectCompileString
		^String.streamContents({ arg stream; this.storeOn(stream); });
	}
	cs { ^this.asCompileString }

	// Other conversions
	-> { |obj| ^Association.new(this, obj) }

	// Reference
	dereference { ^this } // see Ref::dereference
	reference { ^Ref.new(this) }
	dereferenceOperand { ^this }

	// More printing
	printClassNameOn { arg stream;
		var title;
		title = this.class.name.asString;
		stream << if((title @ 0).isVowel, { "an " }, { "a " }) << title;
	}
	printOn { arg stream;
		this.printClassNameOn(stream);
	}
	storeOn { arg stream;
		stream << this.class.name;
		this.storeParamsOn(stream);
		this.storeModifiersOn(stream);
	}
	storeParamsOn { arg stream;
		var args = this.storeArgs;
		if(args.notEmpty) {
			stream << "(" <<<* this.simplifyStoreArgs(args) << ")";
		} {
			stream << ".new"
		}
	}
	simplifyStoreArgs { arg args;
		var res = Array.new, newMethod, methodArgs;
		newMethod = this.class.class.findRespondingMethodFor(\new);
		methodArgs = newMethod.prototypeFrame.drop(1);
		args.size.reverseDo { |i|
			if(methodArgs[i] != args[i]) {
				^args.keep(i + 1)
			}
		}
		^[]
	}
	storeArgs { ^#[] }
	storeModifiersOn { arg stream;}


	// Stream
	next { ^this }
	reset { ^this }
	first { arg inval; this.reset; ^this.next(inval) }
	iter { ^OneShotStream(this) }
	stop { ^this }
	free { ^this }
	clear { ^this }
	removedFromScheduler { ^this }
	embedInStream { ^this.yield; }
	repeat { arg repeats = inf; ^Pn(this, repeats).asStream }
	loop { ^this.repeat(inf) }
	nextN { |n, inval| ^Array.fill(n, { this.next(inval) }) }
	streamArg { arg embed = false;
		^if(embed) {
			Routine { arg inval; this.embedInStream(inval) }
		} {
			Routine { arg inval; loop { inval = this.next(inval).yield } }
		}
	}
	cyc { |n = inf|
		^r { |inval|
			n.do {
				inval = this.embedInStream(inval);
				this.reset;
			}
		}
	}
	fin { |n = 1|
		^r {|inval|
			var item;
			n.do {
				item = this.next(inval);
				if (item.isNil) { nil.alwaysYield };
				inval = item.yield
			}
		}
	}

	// Scheduling.
	awake { arg beats, seconds, clock;
		var time;
		time = seconds; // prevent optimization
		^this.next(beats)
	}
	beats_ {  } // for PauseStream
	clock_ {  } // for Clock

	// UGen related methods
	source { ^this }
	numChannels { ^1 }
	writeDefFile { |name, dir, overwrite=true|
		StartUp.defer { // make sure the synth defs are written to the right path
			var file;
			dir = dir ? SynthDef.synthDefDir;
			if (name.isNil or: { name.asString.isEmpty }) { Error("missing SynthDef file name").throw } {
				name = dir +/+ name ++ ".scsyndef";
				if(overwrite or: { pathMatch(name).isEmpty })
				{
					file = File(name, "w");
					protect {
						AbstractMDPlugin.clearMetadata(name);
						this.asArray.writeDef(file);
					}{
						file.close;
					}
				}
			}
		}
	}


	// Arrays and iteration.
	rank { ^0 }
	size { ^0 }
	indexedSize { ^0 }
	flatSize { ^1 }
	containsSeqColl { ^false }
	do { arg function; function.value(this, 0) }
	generate { arg function, state; this.do(function); ^state }
	! { |n| ^this.dup(n) }
	dup { |n = 2|
		var array;
		if(n.isSequenceableCollection) { ^Array.fillND(n, { this.copy }) };
		array = Array(n);
		n.do {|i| array.add(this.copy) };
		^array
	}

	deepCollect { arg depth, function, index = 0, rank = 0; ^function.value(this, index, rank) }
	deepDo { arg depth, function, index = 0, rank = 0; function.value(this, index, rank) }
	slice { ^this }
	shape { ^nil }
	unbubble { ^this }
	bubble { arg depth=0, levels=1;
		if (levels <= 1) { ^[this] };
		^[this.bubble(depth,levels-1)]
	}

	// Compatibility with SequenceableCollection
	obtain { arg index, default;  ^if(index == 0) { this } { default } }
	instill { arg index, item, default;
		^if(index == 0) { item } {
			this.asArray.instill(index, item, default)
		}
	}


	// Other perform* methods
	// TODO: message should be avoided because it doesn't allow `doesNotUnderstand` to take effect.
	tryPerform { |  ... args, kwargs|
		^if(this.respondsTo(args[0])) {
			this.performArgs(args[0],  args[1..], kwargs)
		}
	}

	multiChannelPerform { |selector ... args|
		^flop([this, selector] ++ args).collect { |item|
			performList(item[0], item[1], item[2..])
		}
	}

	performWithEnvir { |selector, envir|
		var argNames, args;
		var method = this.class.findRespondingMethodFor(selector);

		if(method.isNil) { ^this.doesNotUnderstand(selector) };

		argNames = method.argNames.drop(1);
		args = method.prototypeFrame.drop(1);
		argNames.do { |name, i|
			var val = envir[name];
			val !? { args[i] = val };
		};

		^this.performArgs(selector, args);
	}

	performKeyValuePairs { |selector, pairs| ^this.performWithEnvir(selector, ().putPairs(pairs)) }

	// Catch binary operators failure.
	performBinaryOpOnSomething { |aSelector, thing, adverb|
		if (aSelector === '==', {
			^false
		},{
			if (aSelector === '!=', {
				^true
			},{
				BinaryOpFailureError(this, aSelector, [thing, adverb]).throw;
		})});
	}
	performBinaryOpOnSimpleNumber { |aSelector, thing, adverb|
		^this.performBinaryOpOnSomething(aSelector, thing, adverb)
	}
	performBinaryOpOnSignal { |aSelector, thing, adverb|
		^this.performBinaryOpOnSomething(aSelector, thing, adverb)
	}
	performBinaryOpOnComplex { |aSelector, thing, adverb|
		^this.performBinaryOpOnSomething(aSelector, thing, adverb)
	}
	performBinaryOpOnSeqColl { |aSelector, thing, adverb|
		^this.performBinaryOpOnSomething(aSelector, thing, adverb)
	}
	performBinaryOpOnUGen { |aSelector, thing, adverb|
		^this.performBinaryOpOnSomething(aSelector, thing, adverb)
	}

	// FunctionList support.
	addFunc { arg ... functions;
		^FunctionList([this] ++ functions)
	}
	removeFunc { arg function; if(this === function) { ^nil } }
	replaceFunc { arg find, replace; if(this === find) { ^replace } }
	addFuncTo { arg variableName ... functions;
		this.perform(variableName.asSetter, this.perform(variableName).addFunc(*functions))
	}
	removeFuncFrom { arg variableName, function;
		this.perform(variableName).removeFunc(function)
	}

	// Environment protocol.
	eventAt { ^nil }
	composeEvents { arg event; ^event.copy }

	// TODO: this method is never overridden in the class library, nor is it called anywhere in the code base.
	// Why is it here?
	finishEvent {}

	// LimitedWriteStream protocol.
	atLimit { ^false }

	threadPlayer {}
	threadPlayer_ {}

	// MatchItem protocol.
	matchItem {|item| ^this === item }
	trueAt { ^false }
	falseAt { ^true }


	// Dependancy support.
	*initClass { dependantsDictionary = IdentityDictionary.new(4); }
	dependants { ^dependantsDictionary.at(this) ?? { IdentitySet.new } }
	changed { arg what ... moreArgs;
		dependantsDictionary.at(this).copy.do({ arg item;
			item.update(this, what, *moreArgs);
		});
	}
	addDependant { arg dependant;
		var theDependants;
		theDependants = dependantsDictionary.at(this);
		if(theDependants.isNil,{
			theDependants = IdentitySet.new.add(dependant);
			dependantsDictionary.put(this, theDependants);
		},{
			theDependants.add(dependant);
		});
	}
	removeDependant { arg dependant;
		var theDependants;
		theDependants = dependantsDictionary.at(this);
		if (theDependants.notNil, {
			theDependants.remove(dependant);
			if (theDependants.size == 0, {
				dependantsDictionary.removeAt(this);
			});
		});
	}
	release { this.releaseDependants }
	releaseDependants {	dependantsDictionary.removeAt(this)	}
	update { |theChanged, theChanger|	} // respond to a change in a model

	// instance specific method support
	addUniqueMethod { arg selector, function;
		var methodDict;
		if(function.isKindOf(Function).not) {
			Error("A method must be defined using a function").throw
		};
		if(uniqueMethods.isNil, { uniqueMethods = IdentityDictionary.new });
		methodDict = uniqueMethods.at(this);
		if (methodDict.isNil, {
			methodDict = IdentityDictionary.new;
			uniqueMethods.put(this, methodDict);
		});
		methodDict.put(selector, function);
	}
	removeUniqueMethods {
		if (uniqueMethods.notNil, {
			uniqueMethods.removeAt(this);
		});
	}
	removeUniqueMethod { arg selector;
		var methodDict;
		if (uniqueMethods.notNil, {
			methodDict = uniqueMethods.at(this);
			if (methodDict.notNil, {
				methodDict.removeAt(selector);
				if (methodDict.size < 1, {
					uniqueMethods.removeAt(this);
				});
			});
		});
	}


	// Math protocol support
	// translate these operators to names the code generator can safely generate in C++
	& { arg that; ^bitAnd(this, that) }
	| { arg that; ^bitOr(this, that) }
	% { arg that; ^mod(this, that) }
	** { arg that; ^pow(this, that) }
	<< { arg that; ^leftShift(this, that) }
	>> { arg that; ^rightShift(this, that) }
	+>> { arg that; ^unsignedRightShift(this, that) }
	<! { arg that; ^firstArg(this, that) }

	blend { arg that, blendFrac = 0.5;
		// blendFrac should be from zero to one
		^this + (blendFrac * (that - this));
	}

	blendAt { arg index, method='clipAt';
		var iMin = index.roundUp.asInteger - 1;
		^blend(this.perform(method, iMin), this.perform(method, iMin+1), absdif(index, iMin));
	}

	blendPut { arg index, val, method='wrapPut';
		var iMin = index.floor.asInteger;
		var ratio = absdif(index, iMin);
		this.perform(method, iMin, val * (1-ratio));
		this.perform(method, iMin + 1, val * ratio);
	}

	pair { arg that; ^[this, that] }
	pairs { arg that;
		var list;
		list = [];
		this.asArray.do {|a|
			that.asArray.do {|b|
				list = list.add(a.asArray ++ b)
			};
		};
		^list;
	}






	//////////// ARCHIVING ////////////

	writeArchive { |pathname| ^this.writeTextArchive(pathname) }
	*readArchive { |pathname| ^this.readTextArchive(pathname) }


	initFromArchive {}
	archiveAsCompileString { ^false }
	archiveAsObject { ^this.archiveAsCompileString.not }
	checkCanArchive {}

	*readTextArchive { |pathname| ^pathname.load }
	writeTextArchive { arg pathname;
		var text = this.asTextArchive;
		var file = File(pathname, "w");
		if(file.isOpen) {
			protect {
				file.write(text);
			} { file.close };
		} {
			MethodError("Could not open file % for writing".format(pathname.asCompileString), this).throw;
		}
	}

	asTextArchive {
		var objects, list, stream, firsttime = true;

		if (this.archiveAsCompileString) {
			this.checkCanArchive;
			^this.asCompileString ++ "\n"
		};

		objects = IdentityDictionary.new;

		this.getContainedObjects(objects);

		stream = CollStream.new;
		stream << "var o, p;\n";

		list = List.newClear(objects.size);
		objects.keysValuesDo {|obj, index| list[index] = obj };

		stream << "o = [";
		list.do {|obj, i|
			var size;
			if (i != 0) { stream << ",  "; };
			if ((i & 3) == 0) { stream << "\n\t" };
			obj.checkCanArchive;
			if (obj.archiveAsCompileString) {
				stream << obj.asCompileString;
			}{
				size = obj.indexedSize;
				stream << obj.class.name << ".prNew";
				if (size > 0) {
					stream << "(" << size << ")"
				};
			};
		};
		stream << "\n];\np = [";
		// put in slots
		firsttime = true;
		list.do {|obj, i|
			var slots;
			if (obj.archiveAsCompileString.not) {
				slots = obj.getSlots;
				if (slots.size > 0) {
					if (firsttime.not) { stream << ",  "; };
					firsttime = false;
					stream << "\n\t// " << obj.class.name;
					stream << "\n\t";
					stream << i << ", [ ";
					if (obj.isKindOf(ArrayedCollection)) {
						slots.do {|slot, j|
							var index;
							if (j != 0) { stream << ",  "; };
							if ((j != 0) && ((j & 3) == 0)) { stream << "\n\t\t" };
							index = objects[slot];
							if (index.isNil) {
								stream << slot.asCompileString;
							}{
								stream << "o[" << index << "]";
							};
						};
					}{
						slots.pairsDo {|key, slot, j|
							var index;
							if (j != 0) { stream << ",  "; };
							if ((j != 0) && ((j & 3) == 0)) { stream << "\n\t\t" };
							stream << key << ": ";
							index = objects[slot];
							if (index.isNil) {
								stream << slot.asCompileString;
							}{
								stream << "o[" << index << "]";
							};
						};
					};
					stream << " ]";
				};
			};
		};
		stream << "\n];\n";

		stream << "prUnarchive(o,p);\n";
		^stream.contents
	}

	getContainedObjects { arg objects;
		if (objects[this].notNil) {^this};
		objects[this] = objects.size;

		if (this.archiveAsCompileString.not) {
			this.slotsDo {|key, slot|
				if (slot.archiveAsObject) {
					slot.getContainedObjects(objects);
				};
			};
		};

	}


	///////////// ENCAPSULATION BREAKING METHODS
	// These are dangerous operations as they break encapsulation and
	// can allow access to slots that should not be accessed because they are private to the
	// virtual machine, such as Frame objects.
	// Use with caution.
	// see counterparts to these in ArrayedCollection

	slotSize { ^this.instVarSize }
	slotAt { |index| ^this.instVarAt(index) } // index can be an integer or symbol.
	slotPut { |index, value| ^this.instVarPut(index, value)} // index can be an integer or symbol.
	slotKey { |index| ^this.class.instVarNames.at(index) } // index must be an integer.
	slotIndex { |key| ^this.class.instVarNames.indexOf(key)	}// key must be a symbol.
	slotsDo { |function|
		this.slotSize.do {|i|
			function.value(this.slotKey(i), this.slotAt(i), i)
		}
	}
	slotValuesDo { |function|
		this.slotSize.do {|i|
			function.value(this.slotAt(i), i)
		}
	}

	// getSlots and setSlots will be used for a new implementation of asCompileString.
	// getSlots stores the keys and values so that if the instance
	// variable order changes, setSlots they will still set the right one.
	getSlots {
		var array;
		array = Array.new(this.slotSize * 2);
		this.slotSize.do {|i|
			array.add(this.slotKey(i));
			array.add(this.slotAt(i));
		};
		^array
	}
	setSlots { arg array;
		array.pairsDo {|key, value|
			this.slotPut(key, value)
		}
	}

	instVarSize { _InstVarSize; ^this.primitiveFailed }
	instVarAt { |index| _InstVarAt;	^this.primitiveFailed }// index can be an integer or symbol.
	instVarPut { |index, item| _InstVarPut; ^this.primitiveFailed } // index can be an integer or symbol.
	instVarHash { |instVarNames|
		var indices, res = this.class.hash;
		if(this.instVarSize == 0) {
			^res
		};
		indices = if(instVarNames.notNil) {
			instVarNames.collect(this.slotIndex(_))
		} {
			(0..this.instVarSize-1)
		};
		indices.do { |i|
			var obj = this.instVarAt(i);
			res = res << 1 bitXor: obj.hash;  // encode slot order by left shifting
		};
		^res
	}

	compareObject { arg that, instVarNames;
		if(this === that,{ ^true });
		// possibly ok if one of us isKindOf the other
		if(this.class !== that.class,{ ^false });
		if(instVarNames.notNil,{
			instVarNames.do({ |varname|
				if(this.instVarAt(varname) != that.instVarAt(varname),{
					^false
				})
			});
		},{
			this.instVarSize.do({ arg i;
				if(this.instVarAt(i) != that.instVarAt(i),{ ^false });
			});
		});
		^true
	}

	///////////// DEBUG TIER METHODS
	// Not used in normal code.

	// GC debug information.
	totalFree { _TotalFree ^this.primitiveFailed }
	largestFreeBlock { _LargestFreeBlock ^this.primitiveFailed }
	gcDumpGrey { _GCDumpGrey ^this.primitiveFailed }
	gcDumpSet { |set| _GCDumpSet ^this.primitiveFailed }
	gcInfo { _GCInfo ^this.primitiveFailed }
	gcSanity { _GCSanity ^this.primitiveFailed }
	canCallOS { _CanCallOS ^this.primitiveFailed }

	help { this.class.asString.help	}

	inspect { ^this.inspectorClass.new(this) }
	inspectorClass { ^ObjectInspector }
	inspector {	^Inspector.inspectorFor(this) }


	// Virtual machine debugging.
	crash {	_HostDebugger; ^this.primitiveFailed }
	stackDepth { _StackDepth; ^this.primitiveFailed	}
	dumpStack {	_DumpStack;	^this.primitiveFailed }
	dumpDetailedBackTrace { _DumpDetailedBackTrace;	^this.primitiveFailed }
	freeze { _ObjectDeepFreeze;	^this.primitiveFailed	}

	// Backtraces
	dumpBackTrace {	_DumpBackTrace; ^this.primitiveFailed }
	getBackTrace { _GetBackTrace; ^this.primitiveFailed }

	// Peeks into memory.
	pointsTo { arg obj; _ObjectPointsTo; ^this.primitiveFailed }
	mutable { _ObjectIsMutable; ^this.primitiveFailed }
	frozen { _ObjectIsPermanent; ^this.primitiveFailed }



	///////////// DEPRECATED METHODS
	// Old. Do not use.

	// old binary archiving
	// this will break if the instance vars change !
	// not recommended
	writeBinaryArchive { |pathname| _WriteArchive; ^this.primitiveFailed }
	*readBinaryArchive { |pathname|	_ReadArchive; ^this.primitiveFailed	}
	asBinaryArchive { _AsArchive; ^this.primitiveFailed }
}
