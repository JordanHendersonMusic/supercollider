TestArgumentNil : UnitTest {
	meth_arg {
		arg a = 1, b(1), c((1)), d(2 - 1), e = 2 - 1;
		^[a, b, c, d, e]
	}
	meth_pipe {
		|a = 1, b 1, c((1)), d(2 - 1), e = (2 - 1)|
		^[a, b, c, d, e]
	}
	test_pos {
		if(Main.versionAtLeast(3, 16)){
			this.assertEquals(this.meth_arg, [1, 1, 1, 1, 1], "arg default");
			this.assertEquals(this.meth_arg(nil, nil, nil, nil, nil), [1, 1, 1, 1, 1], "arg explicit nil");
			this.assertEquals(this.meth_pipe, [1, 1, 1, 1, 1], "pipe default");
			this.assertEquals(this.meth_pipe(nil, nil, nil, nil, nil), [1, 1, 1, 1, 1], "pipe explicit nil");
		}
	}
	test_kw {
		if(Main.versionAtLeast(3, 16)){
			this.assertEquals(this.meth_arg(a: nil, b: nil, c: nil, d: nil, e: nil), [1, 1, 1, 1, 1], "arg kw");
				this.assertEquals(this.meth_pipe(a: nil, b: nil, c: nil, d: nil, e: nil), [1, 1, 1, 1, 1], "pipe kw");
		}
	}
	test_kw_replace {
		if(Main.versionAtLeast(3, 16)){
			this.assertEquals(this.meth_arg(12, 12, 12, 12, 12, a: nil, b: nil, c: nil, d: nil, e: nil), [1, 1, 1, 1, 1], "arg kw replace");
			this.assertEquals(this.meth_pipe(12, 12, 12, 12, 12, a: nil, b: nil, c: nil, d: nil, e: nil), [1, 1, 1, 1, 1], "pipe kw replace");
		}
	}

	test_ex_pos {
		if(Main.versionAtLeast(3, 16)){
			this.assertEquals(this.meth_arg(*[nil, nil, nil, nil, nil]), [1, 1, 1, 1, 1], "arg ex");
			this.assertEquals(this.meth_pipe(*[nil, nil, nil, nil, nil]), [1, 1, 1, 1, 1], "pipe ex");
		}
	}

	r_arg { |a = 1| ^a }
	test_r_pos {
		if(Main.versionAtLeast(3, 16)){
			this.assertEquals(this.r_arg, 1, "r arg default");
			this.assertEquals(this.r_arg(nil), 1, "r arg explicit nil");
			this.assertEquals(this.r_arg(a: 2), 2, "r arg new value");
			this.assertEquals(this.r_arg(2, a: nil), 1, "r arg kw nil resets to default");
		}
	}
}
