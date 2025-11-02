TestMethodLookupParent {
	*meow { ^\meow }
	woof { ^\woof }
}

TestMethodLookupBase : TestMethodLookupParent {

}

TestMethodLookup : UnitTest {
	test_class_methods_resolve {
		this.assertEquals(TestMethodLookupParent.meow, \meow);
		this.assertEquals(TestMethodLookupParent.meow, TestMethodLookupBase.meow);
	}
	test_methods_resolve {
		this.assertEquals(TestMethodLookupParent().woof, \woof);
		this.assertEquals(TestMethodLookupParent().woof, TestMethodLookupBase().woof);
	}
}
