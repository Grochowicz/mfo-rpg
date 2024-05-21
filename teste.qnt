module battle {
	type Monster = {
		name: str,
		hp: int,
		dead: bool
	}

	var enemy: Monster

	action init = {
		enemy' = { name:"abc", hp:200, dead:false }
	}

	pure def max(a,b) = if(a>b) a else b

	pure def damage(q: Monster, x: int): Monster = {
		{...q, hp: max(0,q.hp - x), dead: if (q.hp<=x) true else false }
	}

	action attack(x: int): bool = all{
		enemy' = damage(enemy,x)
	}

	action step = any {
		attack(50),
		attack(100)
	}

	val deadifzero = if(enemy.dead) enemy.hp==0 else enemy.hp>0
}
