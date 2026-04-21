TestNonLocalReturn : UnitTest {
	test_try {
		var before = this.stackDepth;
		var after;
		var f;

		try { f.(1, 2, 3, 4, 5, Error("blah").throw ) };

		after = this.stackDepth;

		this.assertEquals(before, after, "Non local returns should not leave things on the stack");
	}

	test_dont_crash {
		var before = this.stackDepth;
		var after;

		// Caused a stack overflow and segfault.
		// see issue 7448
		1000.do{  try {  ()[nil[\a]] }  };

		after = this.stackDepth;

		this.assertEquals(before, after, "Non local returns should not leave things on the stack");
	}

	test_old_issue_from_2012 {
		// see issue 232
		var before = this.stackDepth;
		var after;
		var x = [nil];
		var r = x.do({ |item|
			try { max(1, x) == 2 }
		});

		after = this.stackDepth;
		this.assertEquals(r, x);
		this.assertEquals(before, after, "Non local returns should not leave things on the stack");
	}
}
