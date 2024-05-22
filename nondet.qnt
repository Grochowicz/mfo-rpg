module x {
	var n: int

	action roll = {
		nondet y = oneOf(1.to(20))
		n' = y
	}
}
