// LISP-like two element cells

Pair : Collection {

	var <>linkDown, <>linkAcross;

	*new { arg linkDown, linkAcross;
		^super.newCopyArgs(linkDown, linkAcross)
	}

	// create from nested array
	*newFrom { arg collection;
		var linkDown = collection.at(0);
		var linkAcross = collection.at(1);
		if(linkDown.isKindOf(Collection)) { linkDown = this.newFrom(linkDown) };
		if(linkAcross.isKindOf(Collection)) { linkAcross = this.newFrom(linkAcross) };
		^this.new(linkDown, linkAcross)
	}

	size {
		var i = 0, link = linkAcross;
		while { link = link.tryPerform('linkAcross'); link.notNil}{
			i = i + 1
		};
		^i
	}

	depth { 
		var i = 0, link = linkDown;
		while { link = link.tryPerform('linkDown'); link.notNil} {
			i = i + 1
		};
		^i
	}

	do { |function|
		var i = 0, link, nextLink, res;
		link = linkAcross;
		while { nextLink = link.tryPerform('linkAcross'); nextLink.notNil} {
			i = i + 1;
			res = function.value(link, i);
			link = nextLink;
		};
		^res
	}

	remove { ^this.shouldNotImplement(thisMethod) }
	add { ^this.shouldNotImplement(thisMethod) }

	traverse { arg function;
		// the default traversal order
		^this.depthFirstPreOrderTraversal(function)
	}

	depthFirstPreOrderTraversal { |function|
		var link, nextLinkDown;
		function.value(this);
		linkDown.tryPerform('depthFirstPreOrderTraversal', function);
		// iterate linkAcross to conserve stack depth
		link = linkAcross;
		while { link.notNil } {
			function.value(link);
			nextLinkDown = link.tryPerform('linkDown');
			nextLinkDown !? { nextLinkDown.tryPerform('depthFirstPreOrderTraversal', function) };
			link = link.tryPerform('linkAcross');
		};
	}

	depthFirstPostOrderTraversal { |function|
		var link, nextLinkDown;
		linkDown.tryPerform('depthFirstPreOrderTraversal', function);
		function.value(this);
		// iterate linkAcross to conserve stack depth
		link = linkAcross;
		while { link.notNil } {
			nextLinkDown = link.tryPerform(\linkDown);
			nextLinkDown !? { nextLinkDown.tryPerform('depthFirstPostOrderTraversal, function') };
			function.value(link);
			link = link.tryPerform('linkAcross');
		};
	}

	storeArgs { arg stream;
		^[linkDown, linkAcross]
	}

	printOn { arg stream;
		stream << this.class.name << "(" <<* this.storeArgs << ")"
	}

	storeOn { arg stream;
		stream << this.class.name << "(" <<<* this.storeArgs << ")"
	}
}
