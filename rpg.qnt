module rpg {
	pure def max(a,b) = if(a>b) a else b
    type Class = Barbarian | Wizard | Cleric | Monster
    type Creature = {
        hp: int,
        atk: int,
		ini: int,
        class: Class
    }

	var v: List[Creature]
	var other: Class->Set[Class]

	// Counting Sort
	pure def make_freq(n) = range(0,n).foldl( [], (acc,i) => acc.append(Set()) )
	pure def freq(x) = x.foldl( make_freq(22), (acc,i) => acc.replaceAt(i.ini, acc[i.ini].union( Set(i) )) )
	pure def append_set(x,y) = y.fold( x, (acc,i) => acc.append(i) )
	pure def freq_to_sorted(x) = range(0,x.length()).foldr( [], (i,acc) => acc.append_set(x[i]) )
	pure def sort(x) = freq_to_sorted(freq(x))

	action init = all {
		nondet i1 = oneOf(1.to(20))
		val p1 = { hp: 20, atk: 10, ini: i1, class: Wizard }

		nondet i2 = oneOf(1.to(20))
		val p2 = { hp: 20, atk: 10, ini: i2, class: Cleric }

		nondet i3 = oneOf(1.to(20))
		val p3 = { hp: 150, atk: 10, ini: i3, class: Barbarian }

		nondet i4 = oneOf(1.to(20))
		val e1 = { hp: 100, atk: 20, ini: i4, class: Monster }

		v' = sort(List(p1,p2,p3,e1)),

		val sm = Set(Monster)
		other' = Map(Monster -> Set(Barbarian,Wizard,Cleric), Barbarian -> sm, Wizard -> sm, Cleric -> sm)

//		enemy' = Map(v[0] -> Set(v[3]), v[1] -> Set(v[3]), v[2] -> Set(v[3]), v[3] -> Set(v[0],v[1],v[2])),
//		val mp = Map(p1 -> Set(e1), p2 -> Set(e1), p3 -> Set(e1), e1 -> Set(p1,p2,p3))
	}

	pure def damage(q: Creature, x: int): Creature = {
		{...q, hp: max(0,q.hp - x) }
	}

	action attack(q: int, x: int): bool = all {
		v' = v.replaceAt(q, damage(v[q],x))
	}

	def enemies(q: int): Set[int] = {
		range(0,v.length()).foldl(Set(), (acc,i) => if(other.get(v[q].class).contains(v[i].class)) acc.union(Set(i)) else acc)
	}

	action step = all {
		nondet t1 = oneOf(enemies(0))
		nondet t2 = oneOf(enemies(1))
		nondet t3 = oneOf(enemies(2))
		nondet t4 = oneOf(enemies(3))

		val t = List(t1,t2,t3,t4)
		v' = range(0,v.length()).foldl( v, (acc,i) => acc.replaceAt(t[i], damage(acc[t[i]], v[i].atk))),

		other' = other
	}
}
