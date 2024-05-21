module rpg {
	pure def max(a,b) = if(a>b) a else b
    type Class = Barbarian | Wizard | Cleric | Monster
    type Creature = {
        hp: int,
        atk: int,
		ini: int,
        class: Class
    }

	var p1: Creature
	var p2: Creature
	var p3: Creature
	var e1: Creature

	var v: List[Creature]

	// pure def intCompare(a: int, b: int): Ordering = {
	// 	if(a<b)
	// 		{LT}
	// 	else if(a>b)
	// 		{GT}
	// 	else {EQ}
	// }

	pure def toSet(x) = x.foldl(Set(), (acc,i) => acc.union(Set(i)))

	pure def make_freq(n) = range(0,n).foldl( [], (acc,i) => acc.append(Set()) )
	pure def freq(x) = x.foldl( make_freq(22), (acc,i) => acc.replaceAt(i.ini, acc[i.ini].union( Set(i) )) )
	pure def append_set(x,y) = y.fold( x, (acc,i) => acc.append(i) )
	pure def freq_to_sorted(x) = range(0,x.length()).foldl( [], (acc,i) => acc.append_set(x[i]) )
	pure def sort(x) = freq_to_sorted(freq(x))

	action init = all {
		p1' = { hp: 20, atk: 10, ini: 1, class: Wizard },
		p2' = { hp: 20, atk: 10, ini: 1, class: Cleric },
		p3' = { hp: 150, atk: 10, ini: 1, class: Barbarian },
		e1' = { hp: 100, atk: 20, ini: 1, class: Monster },
		v' = sort(List(p1, p2, p3, e1))
	}

	pure def damage(q: Creature, x: int): Creature = {
		{...q, hp: max(0,q.hp - x) }
	}

	action attack(q: int, x: int): bool = all {
		v' = v.replaceAt(q, damage(v[q],x))
	}
}
