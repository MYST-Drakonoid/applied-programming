class helpers {

    //TODO:damage calculator


    enum class DamageTypes{
        Acid, Bludgeoning, Cold, Fire, Force, Lightning, Necrotic, Piercing, Poison, Psychic, Radiant, Slashing, Thunder

    }

    enum class ConditionTypes(val description: String) {
        Blinded("Cannot see, attack rolls against have advantage, own attacks have disadvantage"),
        Charmed("Cannot attack charmer or target charmer with harmful effects"),
        Deafened("Cannot hear, fails ability checks requiring hearing"),
        Frightened("Disadvantage on ability checks and attacks, cannot move closer to source of fear"),
        Grappled("Speed 0 until grappler releases or distance increases"),
        Incapacitated("Cannot take actions or reactions"),
        Invisible("Cannot be seen without special senses, attacks against have disadvantage, own attacks have advantage"),
        Paralyzed("Cannot move, fails Strength/Dex saves, attacks against have advantage, critical hits within 5 feet"),
        Petrified("Turned to stone, incapacitated, immune to poison/disease, attacks against have advantage, resistance to all damage"),
        Poisoned("Disadvantage on attack rolls and ability checks"),
        Prone("Can only crawl, attacks against have advantage if within 5 feet, own attacks have disadvantage"),
        Restrained("Speed 0, attacks against have advantage, own attacks have disadvantage, disadvantage on Dexterity saves"),
        Stunned("Incapacitated, cannot move, fails Strength/Dex saves, attacks against have advantage"),
        Unconscious("Incapacitated, drops items, fails Strength/Dex saves, attacks against have advantage, melee crits within 5 feet");

        // Getter that prepends the name
        fun getFullDescription(): String = "$name: $description"
    }


    class Stats  (
        val str: Int = 0,
        val dex: Int = 0,
        val con: Int = 0,
        val int: Int = 0,
        val wis: Int = 0,
        val cha: Int = 0
    ) {
        fun get(ability: AbilityScores): Int {
            return when (ability) {
                AbilityScores.Strength -> str
                AbilityScores.Dexterity -> dex
                AbilityScores.Constitution -> con
                AbilityScores.Intelligence -> int
                AbilityScores.Wisdom -> wis
                AbilityScores.Charisma -> cha
            }
        }
    }

    enum class AbilityScores {
        Strength,Dexterity,Constitution,Intelligence,Wisdom,Charisma
    }

    enum class ProficienciesSkill(
        val abilityType: AbilityScores,
        val description: String = ""
    ) {
        Acrobatics(AbilityScores.Dexterity, "Used for balancing, tumbling, and acrobatic feats"),
        Animal_Handling(AbilityScores.Wisdom, "Used for calming or controlling animals"),
        Arcana(AbilityScores.Intelligence, "Used for magical knowledge and identifying spells"),
        Athletics(AbilityScores.Strength, "Used for climbing, swimming, and jumping"),
        Deception(AbilityScores.Charisma, "Used for lying or misleading others"),
        History(AbilityScores.Intelligence, "Used to recall historical facts"),
        Insight(AbilityScores.Wisdom, "Used to sense motives or detect lies"),
        Intimidation(AbilityScores.Charisma, "Used to influence others through threats"),
        Investigation(AbilityScores.Intelligence, "Used to search for clues and solve mysteries"),
        Medicine(AbilityScores.Wisdom, "Used to stabilize or treat the sick or injured"),
        Nature(AbilityScores.Intelligence, "Used to recall knowledge about terrain, plants, and animals"),
        Perception(AbilityScores.Wisdom, "Used to notice hidden objects, creatures, or danger"),
        Performance(AbilityScores.Charisma, "Used for entertaining others"),
        Persuasion(AbilityScores.Charisma, "Used to influence or negotiate"),
        Religion(AbilityScores.Intelligence, "Used to recall information about deities and rituals"),
        Sleight_of_Hand(AbilityScores.Dexterity, "Used for picking pockets or manipulating objects"),
        Stealth(AbilityScores.Dexterity, "Used for sneaking or hiding"),
        Survival(AbilityScores.Wisdom, "Used for tracking, hunting, and surviving in the wild")
    }







    companion object {
        fun monsterProficiency(cr: Int): Int {
            return when (cr) {
                in 0..4 -> 2
                in 5..8 -> 3
                in 9..12 -> 4
                in 13..16 -> 5
                in 17..20 -> 6
                else -> 6
            }
        }
    }
}