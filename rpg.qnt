module rpg {
	pure def max(a,b) = if(a>b) a else b
    type Class = Barbarian | Wizard | Cleric | Monster
    type Creature = {
        hp: int,
        atk: int,
		ini: int,
		paralized: bool,
		taunted: bool,
		who_taunted: int,
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
		val p1 = { hp: 20, atk: 10, ini: i1, paralized: false, taunted: false, who_taunted: -1, imune: false, class: Wizard }

		nondet i2 = oneOf(1.to(20))
		val p2 = { hp: 20, atk: 10, ini: i2, paralized: false, taunted: false, who_taunted: -1, imune: false, class: Cleric }

		nondet i3 = oneOf(1.to(20))
		val p3 = { hp: 150, atk: 10, ini: i3, paralized: false, taunted: false, who_taunted: -1, imune: false, class: Barbarian }

		nondet i4 = oneOf(1.to(20))
		val e1 = { hp: 100, atk: 10, ini: i4, paralized: false, taunted: false, who_taunted: -1, imune: false, class: Monster }
		// monstro começa com 10 de ataque

		v' = sort(List(p1,p2,p3,e1)),

		current' = 0,
		first_step' = true,

		val sm = Set(Monster)
		other' = Map(Monster -> Set(Barbarian,Wizard,Cleric), Barbarian -> sm, Wizard -> sm, Cleric -> sm)
	}

	pure def damage(q: Creature, x: int): Creature = {
		{...q, hp: max(0,q.hp - x) }
	}

	pure def paralize(q: Creature): Creature = {
		{...q, paralized: true }
	}

	pure def unparalize(q: Creature): Creature = {
		{...q, paralized: false}
	}

	pure def taunt(q: Creature, who: int): Creature = {
		{...q, taunted: true, who_taunted: who }
	}

	pure def untaunt(q: Creature): Creature = {
		{...q, taunted: false, who_taunted: -1 }
	}

	pure def immunize(q: Creature): Creature = {
		{...q, imune: true }
	}

	pure def unimmunize(q: Creature): Creature = {
		{...q, imune: false }
	}

	action is_dead(q: int): bool = all {
		v[q].hp == 0,
		v' = v
	}

	action is_paralized(q: int): bool = all {
		v[q].paralized == true,
		v' = v
	}

	action is_taunted_and_attack(q: int): bool = all {
		v[q].taunted == true,
		match v[q].class {
			| Monster => true
			| _ => false
		},
		val p = v[q].who_taunted
		val v2 = v.replaceAt(p, damage(v[p], v[q].atk))
		v' = v2.replaceAt(q, untaunt(v[q]))
	}

	action attack_damage(p: int, q: int): bool = all {
		all {
			v[p].hp > 0,
			v[p].paralized == false,

			v[q].hp > 0,
			v[q].imune == false,
			v' = v.replaceAt( q, damage(v[q], v[p].atk) )
		}
	}

	action attack_paralize(p: int, q: int): bool = all {
		all {
			v[p].hp > 0,
			v[p].paralized == false,
			match v[p].class {
				| Monster => true
				| Wizard => true
				| _ => false
			},

			v[q].hp > 0,
			v[q].paralized == false,

			v' = v.replaceAt( q, paralize(v[q]) )
		}
	}
	action attack_taunt(p: int, q: int): bool = all {
		all {
			v[p].hp > 0,
			v[p].paralized == false,
			match v[p].class {
				| Barbarian => true
				| _ => false
			},

			v[q].hp > 0,
			match v[q].class {
				| Monster => true
				| _ => false
			},

			v' = v.replaceAt( q, taunt(v[q], p) )
		}
	}

	action support_unparalize(p: int, q: int): bool = all {
		all {
			v[p].hp > 0,
			v[p].paralized == false,
			match v[p].class {
				| Monster => false
				| _ => true
			},
			
			v[q].hp > 0,
			match v[q].class {
				| Monster => false
				| _ => true
			},
			v[q].paralized == true,

			v' = v.replaceAt( q, unparalize(v[q]) )
		}
	}

	action support_immunize(p: int, fr: Set[int]): bool = all {
		all {
			v[p].hp > 0,
			v[p].paralized == false,
			match v[p].class {
				| Cleric => true
				| _ => false
			},

			v' = fr.fold(v, (acc,i) => v.replaceAt(i, immunize(v[i])))
		}
	}

	action self_unparalize(p: int): bool = all {
		all {
			v[p].hp > 0,
			v[p].paralized == true,
			match v[p].class {
				| Monster => true
				| _ => false
			},

			v' = v.replaceAt( p, unparalize(v[p]) )
		}
	}

	action cleric_unimmunize(p: int, fr: Set[int]): bool = all {
		all {
			v[p].hp > 0,
			v[p].paralized == false,
			match v[p].class {
				| Cleric => true
				| _ => false
			},

			v' = fr.fold(v, (acc,i) => v.replaceAt(i, unimmunize(v[i])))
		}
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

		// any {
		// 	self_unparalize(), // pensar como fazer
		// 	is_player()
		// }

		// cleric_unimmunize(current, friends(current)), // ok, cleric

		nondet enemy = oneOf(enemies(current).filter(x => true))
		nondet friend = oneOf(friends(current).filter(x => true))
		any {
			is_dead(current), // ok
			is_paralized(current), // ok
			is_taunted_and_attack(current), // ok
			attack_damage(current, enemy), // ok, all
			attack_paralize(current, enemy), // ok, monster, wizard
			attack_taunt(current, enemy), // ok, barbarian
			support_unparalize(current, friend), // ok, all players / monster 
			support_immunize(current, friends(current)), // almost ok, cleric 
			self_unparalize(current), // ok, monster
		},
		//v' = v.replaceAt(enemy, modify(v[enemy], v[current])),

		// val t = List(t1,t2,t3,t4)
		// v' = range(0,v.length()).foldl( v, (acc,i) => acc.replaceAt(t[i], damage(acc[t[i]], v[i].atk))),

		other' = other,
		first_step' = false, // ver se eh a first step pra settar o atk do monstro
		current' = (current + 1) % v.length()
	}
}
