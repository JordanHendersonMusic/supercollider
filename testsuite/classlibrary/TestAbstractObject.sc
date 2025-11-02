TestAbstractObject : UnitTest {
	// Note that some of these tests cannot be written as AbstractObject isn't a real object, and therefore doesn't work in many common sc contexts, such as, print to the interpreter, or using it is a 'try' block.

	test_dont_crash {
		// Can't wrap this in a this.assertNoException as AbstractObject doesn't support isException.
		AbstractObject();
	}

	test_object_superclass {
		this.assertEquals(Object.superclass, AbstractObject);
	}
	test_abstract_object_superclass {
		this.assertEquals(AbstractObject.superclass, nil)
	}
	test_abstract_object_class {
		this.assertEquals(AbstractObject.class, Meta_AbstractObject)
	}
	test_meta_abstract_object_class {
		this.assertEquals(Meta_AbstractObject.class, Class)
	}

	test_identity {
		var o = AbstractObject();
		var i = AbstractObject();

		this.assert(o === o);
		this.assert(o !== i);

		// This method cannot be written because isException isn't implemented.
		// this.assertException({ o == i }, DoesNotUnderstandError, "AbstractObject should not support equality by itself.");
	}

	test_copy {
		var a = AbstractObject();

		this.assert(a.identityHash != AbstractObject().identityHash, "AbstractObjects should have unique hashes.");

	}
}
