

fun main() {
    val testMonster = monster_tracker.Monster(
        name = "Goblin",
        creatureType = "Humanoid",
        speed = 30,
        stat = helpers.Stats(10, 14, 10, 10, 8, 8),
        initiative = 2,
        ac = 15,
        hp = 12,
        cr = 1,
        attack = "1d20 dex slashing",
        weaknesses = mutableSetOf(),
        defaultResistances = arrayOf(),
        immunities = mutableSetOf(),
        conImmunities = arrayOf(),
        conditions = mutableSetOf(),
        skillProf = arrayOf(),
        saveProf = arrayOf(),
        otherNotes = ""
    )

    CombatDashboard(testMonster)
}