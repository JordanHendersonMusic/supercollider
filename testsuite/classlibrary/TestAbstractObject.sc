TestExperimentalAbstractObject : UnitTest {
	// Note that some of these tests cannot be written as ExperimentalAbstractObject isn't a real object, and therefore doesn't work in many common sc contexts, such as, print to the interpreter, or using it is a 'try' block.

	test_dont_crash {
		// Can't wrap this in a this.assertNoException as ExperimentalAbstractObject doesn't support isException.
		ExperimentalAbstractObject();
	}

	test_object_superclass {
		this.assertEquals(Object.superclass, ExperimentalAbstractObject);
	}
	test_abstract_object_superclass {
		this.assertEquals(ExperimentalAbstractObject.superclass, nil)
	}
	test_abstract_object_class {
		this.assertEquals(ExperimentalAbstractObject.class, Meta_ExperimentalAbstractObject)
	}
	test_meta_abstract_object_class {
		this.assertEquals(Meta_ExperimentalAbstractObject.class, Class)
	}

	test_identity {
		var o = ExperimentalAbstractObject();
		var i = ExperimentalAbstractObject();

		this.assert(o === o);
		this.assert(o !== i);

		// This method cannot be written because isException isn't implemented.
		// this.assertException({ o == i }, DoesNotUnderstandError, "ExperimentalAbstractObject should not support equality by itself.");
	}

	test_copy {
		var a = ExperimentalAbstractObject();
		this.assert(a.identityHash != ExperimentalAbstractObject().identityHash, "ExperimentalAbstractObjects should have unique hashes, technically this could be a false negative, but seems unlikely.");
	}
}
