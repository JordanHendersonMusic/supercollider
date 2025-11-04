Association : Magnitude {
	var <>key, <>value;

	*new { arg key, value;
		^super.newCopyArgs(key, value)
	}

	== { |other|
		var otherKey = other.tryPerform(\key); 
		^otherKey !? { otherKey == key } ?? { false }
	}

	hash {
		^key.hash
	}

	< { arg anAssociation;
		^key < anAssociation.key
	}

	printOn { arg stream;
		stream << "(" << key << " -> " << value << ")";
	}
	storeOn { arg stream;
		stream << "(" <<< key << " -> " <<< value << ")";
	}

	// Pattern support
	embedInStream { arg inval;
		^inval.add(this).yield;
	}
	transformEvent { arg event;
		^event.add(this);
	}
}
