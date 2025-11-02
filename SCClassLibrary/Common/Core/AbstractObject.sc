// AbstractObject is often not valid in many standard sc contexts, e.g., it doesn't support asString so can't be shown in the post window.
// As a result, it isn't supposed to be used directly, but as a way to implement other objects that do work in standard sc contexts, ProtoObject and AbstractWrapper both do this.
// There is also an issue with 'keywords', sc lang doesn't have any, but being able to 'yield' or 'copy' an object isn't really a part of the object interface, but more a fundamental feature of how users expect the language to work.
// This is a fundamental awkwardness with smalltalk and strictly OOP languages.
// For this reason, a few of these methods are implemented here, again, AbstractObject is not supposed to be used by itself, but inherited from — AbstractObject is an *incomplete* object.
// There is an additional issue with primitives, to help combat this the following 'sc_abstract_object*' methods are introduced to easily enable abstract objects to opt into certain expected behaviour,
//     note, this doesn't cover all standard sc behaviour, just cases that are impossible to implement otherwise.
// Deciding what is and isn't 'standard sc behaviour' is left up to those who subclass AbstractObject:
//     maximally, it could be every single method in Object;
//     minimally, it is recommend to support 'asString', 'dump', and 'class' otherwise error printing will fail.


// There are three methods from Object that absolutely cannot be overridden and are key parts of the interpreter:
//    identityHash, ===, and !==, these define the concept of identity, which is essentially memory address.

// No additional normal methods should ever be written here.
// !!! --- PLEASE NEVER ADD A NORMAL PASCAL CASE METHOD TO ABSTRACT OBJECT --- !!!
// The whole point of this class is to be minimal and incomplete, allowing those who subclass it to create their own definition of what an object is and isn't.

// The following messages are requirements to subclass from AbstractObject, default implementations are provided.
// Note, they do not have to be implemented as messages, as doesNotUnderstand will be obeyed, see ProtoObject.
//    1. doesNotUnderstand { 1.exit }
//          When the object doesn't understand a message, the interpreter will forcibly call this method, if it doesn't exist, you will get a segfault (hopefully).
//    2. mustBeBoolean { this.sc_abstract_object_must_be_boolean }
//          When doing `if(a) {..} {...}` for all a <: AbstractObject, the interpreter will try to call mustBeBoolean.
//          If it doesn't exist, you will (hopefully) get a segfault.
//          Additionally, this message is REQUIRED to throw an error as the jump byte codes have no way to skip the true and false branches.
AbstractObject {
	*new { |maxSize = 0| _BasicNew; ^this.sc_abstract_object_primitive_failed }
	*newCopyArgs { | ... args, kwargs | _BasicNewCopyArgsToInstVars; ^this.sc_abstract_object_primitive_failed }

	// Hashing, AbstractObject support identity, this is required by the interpreter and cannot actually be overridden.
	// When you put AbstractObjects into an identity dictionary the interpreter uses its own implementation, this should match or things will get confusing.
	identityHash { _ObjectHash; ^this.sc_abstract_object_primitive_failed }
	// Equality operators are provided, this isn't strictly necessary, but we usually think of identity and these operators as a set.
	=== { arg obj; _Identical; ^this.sc_abstract_object_primitive_failed }
	!== { arg obj;_NotIdentical; ^this.sc_abstract_object_primitive_failed }

	// AbstractObject does NOT support dependencies (it can be added in a subclass but must register with Object.dependantsDictionary)
	// AbstractObject does NOT support unique methods, these are easy to implement in a subclass, and unnecessary in ProtoObject.
	// AbstractObject does NOT support gc reporting nor does it expose any compiler internal methods.
	// AbstractObject does NOT support archiving, this can be implemented in the subclass.
	// AbstractObject does NOT support switch as it requires equality.

	// Helper methods and primitive implementations.

	// These errors don't actually pass on 'this', this is because printing 'this' requires the object to be able to respond.
	// If we passed 'this', it is likely it get the interpreter stuck in an infinite loop trying to print an error.
	// You may wish to reimplement these to pass 'this' if you know that printing is a safe operation.
	sc_abstract_object_primitive_failed { PrimitiveFailedError("AbstractObject").throw }
	sc_abstract_object_must_be_boolean { MustBeBooleanError(nil, "AbstractObject").throw; }

	sc_abstract_object_hash { _ObjectHash; ^this.sc_abstract_object_primitive_failed }
	sc_abstract_object_copy_shallow { _ObjectShallowCopy; ^this.sc_abstract_object_primitive_failed }
	sc_abstract_object_copy_immutable { _ObjectCopyImmutable; ^this.sc_abstract_object_primitive_failed }
	sc_abstract_object_copy_deep { _ObjectDeepCopy; ^this.sc_abstract_object_primitive_failed }

	// Yielding — must be implemented as primitives as special methods are provided.
	// Note that idle is not provided because it requires calling 'value' on the AbstractObject'.
	sc_abstract_object_yield {	_RoutineYield; ^this.sc_abstract_object_primitive_failed }
	sc_abstract_object_always_yield { _RoutineAlwaysYield; ^this.sc_abstract_object_primitive_failed }
	sc_abstract_object_yield_and_reset { |reset = true| _RoutineYieldAndReset; ^this.sc_abstract_object_primitive_failed }

	// This is often necessary in abstract objects to determine if we have an abstract object, use isKindOf in normal use.
	sc_abstract_object_is_kind_of { |aClass| _ObjectIsKindOf; ^this.sc_abstract_object_primitive_failed }

	// Needed when we want to do the normal method calling in an AbstractObject.
	// We do not provide 'tryPerform' as it required 'respondsTo', which is bad design as it doesn't allow 'doesNotUnderstand' to take effect.
	// Since 'multiChannelPerform' and 'performWithEnvir' do not require primitives, they are not provided either.
	// In most cases, the 'perform*' messages should defer to these as most of the time, you actually want to implement doesNotUnderstand.
	sc_abstract_object_perform_args { |selector, args, kwargs|	_ObjectPerformArgs;	^this.sc_abstract_object_primitive_failed }
	sc_abstract_object_super_perform_args { |selector, args, kwargs| _ObjectSuperPerformArgs; ^this.sc_abstract_object_primitive_failed }
	sc_abstract_object_perform_msg { |msg| _ObjectPerformMsg; ^this.sc_abstract_object_primitive_failed }
	sc_abstract_object_perform { |selector ... args| _ObjectPerform; ^this.sc_abstract_object_primitive_failed }
	sc_abstract_object_perform_list { | ...args, kwargs| _ObjectPerformList; ^this.sc_abstract_object_primitive_failed }
}
