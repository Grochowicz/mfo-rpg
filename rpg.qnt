module rpg {
	pure def max(a,b) = if(a>b) a else b
    type Class = Barbarian | Wizard | Cleric | Monster
    type Creature = {
        hp: int,
        atk: int,
		spd: int,
        class: Class
    }

	var p1: Creature
	var p2: Creature
	var p3: Creature
	var e1: Creature

	var v: List[Creature]

	action init = all {
		p1' = { hp: 20, atk: 10, spd: 1, class: Wizard },
		p2' = { hp: 20, atk: 10, spd: 1, class: Cleric },
		p3' = { hp: 150, atk: 10, spd: 1, class: Barbarian },
		e1' = { hp: 100, atk: 20, spd: 1, class: Monster },
		v' = List(p1,p2,p3,e1)
	}

	pure def damage(q: Creature, x: int): Creature = {
		{...q, hp: max(0,q.hp - x) }
	}

	action attack(q: int, x: int): bool = all {
		v' = v.replaceAt(q, damage(v[q],x))
	}
}
