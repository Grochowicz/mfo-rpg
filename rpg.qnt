module rpg {
	pure def max(a,b) = if(a>b) a else b
    type Class = Barbarian | Wizard | Cleric | Monster
    type Creature = {
        hp: int,
        atk: int,
		ini: int,
		paralized: bool,
		taunted: bool,
		imune: bool,
        class: Class
    }

	var first_step: bool
	var current: int
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
		val p1 = { hp: 20, atk: 10, ini: i1, paralized: false, taunted: false, imune: false, class: Wizard }

		nondet i2 = oneOf(1.to(20))
		val p2 = { hp: 20, atk: 10, ini: i2, paralized: false, taunted: false, imune: false, class: Cleric }

		nondet i3 = oneOf(1.to(20))
		val p3 = { hp: 150, atk: 10, ini: i3, paralized: false, taunted: false, imune: false, class: Barbarian }

		nondet i4 = oneOf(1.to(20))
		val e1 = { hp: 100, atk: 10, ini: i4, paralized: false, taunted: false, imune: false, class: Monster }
		// monstro começa com 10 de ataque

		v' = sort(List(p1,p2,p3,e1)),

		current' = 0,
		first_step' = true,

		val sm = Set(Monster)
		other' = Map(Monster -> Set(Barbarian,Wizard,Cleric), Barbarian -> sm, Wizard -> sm, Cleric -> sm)

//		enemy' = Map(v[0] -> Set(v[3]), v[1] -> Set(v[3]), v[2] -> Set(v[3]), v[3] -> Set(v[0],v[1],v[2])),
//		val mp = Map(p1 -> Set(e1), p2 -> Set(e1), p3 -> Set(e1), e1 -> Set(p1,p2,p3))
	}

	pure def is_dead(q: Creature): bool = q.hp <= 0

	pure def is_paralized(q: Creature): bool = q.paralized == true

	pure def damage(q: Creature, x: int): Creature = {
		{...q, hp: max(0,q.hp - x) }
	}

	action attack(q: int, x: int): bool = all {
		// como faz pra checkar as condicoes pra ver se eu posso atacar?
		v' = v.replaceAt(q, damage(v[q],x))
	}

	action paralize(q: int): bool = all {
		// aqui tambem, como eu checko se posso paralizar o v[q]?
		v' = v.replaceAt(q, { ...v[q], paralized: true })
	}

	def enemies(q: int): Set[int] = {
		range(0,v.length()).foldl(Set(), (acc,i) => if(other.get(v[q].class).contains(v[i].class)) acc.union(Set(i)) else acc)
	}

	def friends(q: int): Set[int] = {
		range(0,v.length()).foldl(Set(), (acc,i) => if(other.get(v[q].class).contains(v[i].class)) acc else acc.union(Set(i)))
	}

	// def modify(q: Creature, p: Creature): Creature = {

	// }

	action step = all {
		// quem joga agora eh o v[current]
		nondet enemy = oneOf(enemies(current))
		nondet friend = oneOf(friends(current))

		any {
			is_dead(v[current]),
			is_paralized(v[current]),
			// attack(enemy, v[current].atk), // NAO FUNCIONA, NAO SEI PQ
			// paralize(enemy), // TAMBEM NAO FUNCIONA, PQQQ???
			// taunt(enemy),
			// unparalize(friend),
		},
		//v' = v.replaceAt(enemy, modify(v[enemy], v[current])),

		// val t = List(t1,t2,t3,t4)
		// v' = range(0,v.length()).foldl( v, (acc,i) => acc.replaceAt(t[i], damage(acc[t[i]], v[i].atk))),

		other' = other,
		first_step' = false, // ver se eh a first step pra settar o atk do monstro
		current' = (current + 1) % v.length()
	}
}
