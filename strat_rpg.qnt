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
	
	type What = Damage | Taunt | Immunize | Paralize | Skip | Unparalize

	type Move = {
		a: int,
		b: int,
		what: What,
		move_damage: int
	}

	var last_move: Move
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
		other' = Map(Monster -> Set(Barbarian,Wizard,Cleric), Barbarian -> sm, Wizard -> sm, Cleric -> sm),
		last_move' = { a: -1, b: -1, what: Damage, move_damage:0 }

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
		match v[q].class {
			| Cleric => false
			| _ => true 
		},
		v[q].hp == 0,
		v' = v,
		last_move' = { a: q, b: -1, what: Skip, move_damage: 0 }
	}

	action is_paralized(q: int): bool = all {
		match v[q].class {
			| Monster => false 
			| _ => true 
		},
		v[q].paralized == true,
		v' = v,
		last_move' = { a: q, b: -1, what: Skip, move_damage: 0 }
	}

	action is_taunted_and_attack(q: int): bool = all {
		v[q].paralized == false,
		v[q].taunted == true,
		match v[q].class {
			| Monster => true
			| _ => false
		},
		val p = v[q].who_taunted

		val dmg = if(first_step) v[q].atk else 2*v[q].atk
		val v2 = v.replaceAt(p, damage(v[p], dmg))
		v' = v2.replaceAt(q, untaunt(v[q])),

		val p = v[q].who_taunted
		val dmg = if(first_step) v[q].atk else 2*v[q].atk
		last_move' = { a: q, b: p, what: Damage, move_damage: dmg }
	}

	action attack_damage(p: int, q: int): bool = all {
		not(can_unparalize(p)),
		not(can_skill(p)),
		v[p].hp > 0,
		v[p].paralized == false,
		v[p].taunted == false,

		match v[p].class {
			| Cleric => false
			| _ => true
		},

		v[q].imune == false,
		val double_damage = not(first_step) and match v[p].class {
			| Monster => true
			| _ => false
		}
		val dmg = if(double_damage) 2*v[p].atk else v[p].atk
		v' = v.replaceAt( q, damage(v[q], dmg) ),

		val double_damage = not(first_step) and match v[p].class {
			| Monster => true
			| _ => false
		}
		val dmg = if(double_damage) 2*v[p].atk else v[p].atk
		last_move' = { a: p, b: q, what: Damage, move_damage: dmg }
	}

	action attack_paralize(p: int, q: int): bool = all {
		not(can_unparalize(p)),
		can_skill(p),
		v[p].hp > 0,
		v[p].paralized == false,
		match v[p].class {
			| Monster => true
			| Wizard => true
			| _ => false
		},

		v[q].hp > 0,
		v[q].paralized == false,

		v' = v.replaceAt( q, paralize(v[q]) ),
		last_move' = { a: p, b: q, what: Paralize, move_damage: 0}
	}
	action attack_taunt(p: int, q: int): bool = all {
		not(can_unparalize(p)),
		can_skill(p),
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

		v' = v.replaceAt( q, taunt(v[q], p) ),
		last_move' = { a: p, b: q, what: Taunt, move_damage: 0}
	}

	action support_unparalize(p: int, q: int): bool = all {
		can_unparalize(p),
		v[p].hp > 0,
		v[p].paralized == false,
		match v[p].class {
			| Monster => false
			| Cleric => false
			| _ => true
		},
		
		v[q].hp > 0,
		match v[q].class {
			| Monster => false
			| _ => true
		},
		v[q].paralized == true,

		v' = v.replaceAt( q, unparalize(v[q]) ),
		last_move' = { a: p, b: q, what: Unparalize, move_damage: 0}
	}

	action self_unparalize(p: int): bool = all {
		v[p].hp > 0,
		v[p].paralized == true,
		match v[p].class {
			| Monster => true
			| _ => false
		},

		v' = v.replaceAt( p, unparalize(v[p]) ),
		last_move' = { a: p, b: p, what: Unparalize, move_damage: 0}
	}

	action cleric_unimmunize_and_attack_or_support(p: int, q: int, fr: Set[int]): bool = all {
		match v[p].class {
			| Cleric => true
			| _ => false
		},

		any {
			all {
				v[p].hp==0, 
				v' = fr.fold(v, (acc,i) => acc.replaceAt(i, unimmunize(v[i]))),
				last_move' = { a: p, b: -1, what: Skip, move_damage: 0}
			},
			all {
				v[p].paralized==true,
				v' = fr.fold(v, (acc,i) => acc.replaceAt(i, unimmunize(v[i]))),
				last_move' = { a: p, b: -1, what: Skip, move_damage: 0}
			},
			all {
				v[p].hp > 0,
				v[p].paralized == false,

				val v2 = fr.fold(v, (acc,i) => acc.replaceAt(i, unimmunize(v[i])))

				any {
					all {
						not(can_unparalize(p)),
						not(can_skill(p)),
						v' = v2.replaceAt( q, damage(v2[q], v2[p].atk) ),
						last_move' = { a: p, b: q, what: Damage, move_damage: v2[p].atk}
					},
					all {
						not(can_unparalize(p)),
						can_skill(p),
						v' = fr.fold(v2, (acc,i) => acc.replaceAt(i, immunize(v2[i]))),
						last_move' = { a: p, b: -1, what: Immunize, move_damage: 0}
					},
					all {
						can_unparalize(p),
						nondet target = oneOf(fr.filter(x => v[x].paralized))
						v' = v.replaceAt( target, unparalize(v[target]) ),
						nondet target = oneOf(fr.filter(x => v[x].paralized))
						last_move' = { a: p, b: target, what: Unparalize, move_damage: 0}
					}
				}
			}
		}
	}

	def enemies(q: int): Set[int] = {
		range(0,v.length()).foldl(Set(), (acc,i) => if(other.get(v[q].class).contains(v[i].class)) acc.union(Set(i)) else acc)
	}

	def friends(q: int): Set[int] = {
		range(0,v.length()).foldl(Set(), (acc,i) => if(other.get(v[q].class).contains(v[i].class)) acc else acc.union(Set(i)))
	}

	def can_unparalize(q: int):bool = {
		val c1 = match v[q].class {
			| Monster => false
			| _ => true  
		}
		val c2 = friends(current).fold( false, (acc,i) => acc or v[i].paralized )
		c1 and c2
	}

	def can_skill(q: int):bool = {
		val c1 = match v[q].class {
			| Monster => false
			| _ => true
		}
		val c2 = not(v[q].paralized)
		val c3 = enemies(current).fold( true, (acc,i) => acc and v[i].paralized )
		val c4 = enemies(current).fold( true, (acc,i) => acc and v[i].taunted)
		c1 and c2 and not(c3 or c4)
	}

	action step = all {
		// quem joga agora eh o v[current]

		nondet enemy = oneOf(enemies(current))
		nondet friend = oneOf(friends(current))
		any {
			is_dead(current), // ok
			is_paralized(current), // ok
			is_taunted_and_attack(current), // ok
			attack_damage(current, enemy), // ok, all except cleric
			attack_paralize(current, enemy), // ok, monster, wizard
			attack_taunt(current, enemy), // ok, barbarian
			cleric_unimmunize_and_attack_or_support(current, enemy, friends(current)),
			self_unparalize(current), // ok, monster
		},

		other' = other,
		first_step' = false, // ver se eh a first step pra settar o atk do monstro
		current' = (current + 1) % v.length()
	}

	val monster_alive = v.foldl( true,(acc, i) => if(match i.class {
		| Monster => true
		| _ => false
		}) acc and i.hp>0 else acc )

	val players_alive = v.foldl( true,(acc, i) => if(match i.class {
		| Monster => false 
		| _ => true 
		}) acc and i.hp>0 else acc )
}
