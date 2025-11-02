TestProtoObject : UnitTest {
	test_basic {
		var p = #(
			numChannels: {|self, r| r },
			dump: { "dump" }
		);

		this.assertEquals(p.numChannels(10), 10);
		this.assertEquals(p.dump, "dump");
		this.assertException({ p.post }, DoesNotUnderstandError);
	}

	test_doesNotUnderstand {
		var p = #(
			doesNotUnderstand: { |self, selector... args, kwargs |
				[selector, args, kwargs]
			}
		);

		this.assertEquals(p.meow(1, foo: 2), [\meow, [1], [\foo, 2]], "Should be able to override 'doesNotUnderstand' on ProtoObject, very powerful, but potentially dangerous!")
	}

	test_nesting {
		var p = #(p: #(meow: 10));
		this.assertEquals(10, p.p.meow, "Nested ProtoObjects should work.");
	}

	test_locking {
		var p = #(protoObjectLock: true);
		this.assertException( {p.foo = 10}, Error, "Should not be able to set a locked protoobject key");
	}

	test_is_nil {
		this.assert(#().isNil.not, "ProtoObject should respect nil as they are still 'things'.");
		this.assert(#().notNil, "ProtoObject should respect nil as they are still 'things'.");
	}

	test_value {
		var p = #();
		this.assert(p.() === p, "Calling value on a ProtoObject should return itself by default.");

		p.value = \meow;
		this.assertEquals(p.(), \meow, "Should be able to override ProtoObject's value method, odd but you should be able to do it");
	}

	test_printing {
		this.assertNoException( { #().postln }, "The interpreter should be able to print ProtoObjects.");
		this.assertNoException( { [#()].postln }, "The interpreter should be able to print ProtoObjects when nested in collections.");
		this.assertNoException( { (p: #()).postln }, "The interpreter should be able to print ProtoObjects when nested in collections.");
	}

	test_equality {
		var p = #();
		this.assert(p === p, "Identical ProtoObject should be identical.");
		this.assert(p !== #(), "Non-identical ProtoObjects should not compare identical");
		this.assertEquals([#(), #()], [#(), #()], "ProtoObjects should compare equal");
	}

	test_in_an_event {
		var e = ();
		var proto = #();
		e[proto] = 1;
		this.assertEquals(e[proto], 1, "Should be able to use ProtoObjects as keys");
	}


	test_super {
		var parent = #(speak: {|self| self.speakImpl });
		var base = #(super: parent, speakImpl: \meow);
		this.assertEquals(base.speak, \meow, "Super should work in ProtoObjects");
	}

	test_many_supers {
		var r = #(speak: {|self| self.speakImpl } );
		10.do { r = #(super: r) };
		r = #(super: r, speakImpl: \meow);
		this.assertEquals(r.speak, \meow, "Super should work in ProtoObjects even when nested deeply");
	}

	test_super_doesNotUnderstand {
		var sss = #();
		var ss = #(super: sss);
		var s = #(super: ss);

		this.assertException( {s.foo}, DoesNotUnderstandError, "Default does not understand implementation should throw");

		s.doesNotUnderstand = \meow;

		this.assertEquals(s.foo, \meow, "Overriding first level of does not understand should work");

		s.doesNotUnderstand = nil;

		this.assertException( {s.foo}, DoesNotUnderstandError, "Default does not understand implementation should throw after being unset");

		ss.doesNotUnderstand = \woof;

		this.assertEquals(s.foo, \woof, "Overriding second level of does not understand works");

		s.doesNotUnderstand = \meow;

		this.assertEquals(s.foo, \meow, "Overriding first level of does not understand should work even though there is a second level too");

		s.doesNotUnderstand = nil;
		ss.doesNotUnderstand = nil;
		sss.doesNotUnderstand = \chirp;

		this.assertEquals(s.foo, \chirp, "Overriding top level of does not understand works");
	}


	test_can_yield {
		var p = #(foo: 10);
		var r = Routine{ p.yield };
		this.assertEquals(r.next.foo, 10, "Should be able to yield a ProtoObject.");
		r = Routine{ p.idle(1) }; // Potentially a bug, if it takes longer than 1 second to evaluate the following lines.
		10.do { |i|
			this.assertEquals(r.next().foo, 10, "Should be able to idle a ProtoObject on iteration %.".format(i), false);
		};
		r = Routine{ p.alwaysYield };
		10.do{
			this.assertEquals(r.next().foo, 10, "Should be able to alwaysYield a ProtoObject.", false)
		};
	}
}
