TestIdentityDictionaryObjectPrototyping : UnitTest {
	test_known {
		var ev = (\a: 1, \b: 2);
		this.assertEquals(ev.a, 1);
		this.assertEquals(ev.b, 2)
	}
	test_known_function_call {
		var ev = (\a: 1, \b: 2, \f: {|self, foo, bar| [self, foo, bar] });
		var result = ev.f(\foo, \bar);
		this.assertEquals(result[0], ev, "'this' should be passed in first");
		this.assertEquals(result[1], \foo);
		this.assertEquals(result[2], \bar);
	}
	test_known_function_call_with_args_exact {
		var ev = (\a: 1, \b: 2, \f: {|self, foo, bar| [self, foo, bar] });
		var result = ev.f(foo: \foo, bar: \bar);
		this.assertEquals(result[0], ev, "'this' should be passed in first");
		this.assertEquals(result[1], \foo);
		this.assertEquals(result[2], \bar);
	}
	test_known_function_call_with_args_mixed_exact {
		var ev = (\a: 1, \b: 2, \f: {|self, foo, bar| [self, foo, bar] });
		var result = ev.f(\foo, bar: \bar);
		this.assertEquals(result[0], ev, "'this' should be passed in first");
		this.assertEquals(result[1], \foo);
		this.assertEquals(result[2], \bar);
	}
	test_known_function_call_forward {
		var ev = (
			\a: 1,
			\b: 2,
			\forward: {|self, selector, a1| [self, selector, a1] }
		);
		var result = ev.fasdfasdfasdfasdf(\foo);
		this.assertEquals(result[0], ev, "'this' should be passed in first");
		this.assertEquals(result[1], \fasdfasdfasdfasdf);
		this.assertEquals(result[2], \foo);
	}
	test_known_function_call_forward_with_keywordArgs {
		var ev = (
			\a: 1,
			\b: 2,
			\forward: {|self, selector, a1| [self, selector, a1] }
		);
		var result = ev.asdfasdfasdff(a1: \foo);
		this.assertEquals(result[0], ev, "'this' should be passed in first");
		this.assertEquals(result[1], \asdfasdfasdff);
		this.assertEquals(result[2], \foo);
	}
}
