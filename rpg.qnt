module rpg {
    //type Creature = Player | Monster
    type Class = Barbarian | Wizard | Cleric
    type Player = {
        name: str,
        health: int,
        attack: int,
        defense: int,
        class: Class
    }
}
