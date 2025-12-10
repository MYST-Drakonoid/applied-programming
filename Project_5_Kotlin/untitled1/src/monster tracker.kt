import java.awt.*
import javax.swing.*
import javax.swing.border.EmptyBorder

class monster_tracker {



    class Monster(
        val name: String,
        val creatureType: String,
        val speed: Int,
        var curSpeed: Int = speed,
        val stat: helpers.Stats,
        var initiative: Int,
        val ac: Int,
        var hp: Int,
        val cr: Int,
        val pb: Int = helpers.monsterProficiency(cr),
        val attack: String,
        val weaknesses: MutableSet<helpers.DamageTypes>,
        val defaultResistances: Array<helpers.DamageTypes>,
        var resistances: MutableSet<helpers.DamageTypes> = defaultResistances.toMutableSet(),
        val immunities: MutableSet<helpers.DamageTypes>,
        val conImmunities: Array<helpers.ConditionTypes>,
        var conditions: MutableSet<helpers.ConditionTypes>,
        var advantage: Boolean = false,
        var disadvantage: Boolean = false,
        val skillProf: Array<helpers.ProficienciesSkill>,
        val saveProf: Array<helpers.AbilityScores>,
        var otherNotes: String) {

        /**
         * Applies incoming damage to the monster, after accounting for
         * weaknesses, resistances, and immunities.
         *
         * @param amount Base damage before modifiers.
         * @param dType The damage type dealt.
         * @return A string describing how effective the attack was.
         */
        fun takeDamage(amount: Int, dType: helpers.DamageTypes): String {

            // Adjust damage based on the monster's traits
            val effectiveDamage = when {
                weaknesses.contains(dType)   -> amount * 2
                resistances.contains(dType)  -> amount / 2
                immunities.contains(dType)   -> 0
                else                         -> amount
            }

            // Apply damage
            hp -= effectiveDamage

            // Kill state
            if (hp <= 0) return "monster death"

            // Feedback text
            return when {
                weaknesses.contains(dType)   -> "Super effective"
                resistances.contains(dType)  -> "not very effective"
                immunities.contains(dType)   -> "no damage"
                else                         -> "damage taken"
            }
        }


        /**
         * Heals the monster for the given amount.
         *
         * @param amount Hit points restored.
         * @return Simple confirmation string.
         */
        fun heal(amount: Int): String {
            hp += amount
            return "healed"
        }

        /**
         * Attempts to apply a new condition to the monster.
         *
         * - Checks immunity
         * - Applies condition effects (speed reduction, advantage/disadvantage, resistances, etc.)
         * - Stores the condition in the active set
         *
         * @param condition The condition to apply.
         * @return A descriptive outcome message.
         */
        fun applyCondition(condition: helpers.ConditionTypes): String {

            // Cannot apply if already active or immune
            if (conditions.contains(condition) || conImmunities.contains(condition)) {
                return when {
                    conditions.contains(condition)   -> "condition is already set"
                    conImmunities.contains(condition) -> "$name is immune to ${condition.name}"
                    else -> "condition addition failed"
                }
            }

            // Add condition to active list
            conditions.add(condition)

            // Apply mechanical effects
            return when (condition) {

                helpers.ConditionTypes.Paralyzed -> {
                    curSpeed = 0
                    condition.getFullDescription()
                }

                helpers.ConditionTypes.Blinded -> {
                    disadvantage = true
                    condition.getFullDescription()
                }

                helpers.ConditionTypes.Charmed ->
                    condition.getFullDescription()

                helpers.ConditionTypes.Deafened ->
                    condition.getFullDescription()

                helpers.ConditionTypes.Frightened -> {
                    disadvantage = true
                    condition.getFullDescription()
                }

                helpers.ConditionTypes.Grappled -> {
                    curSpeed = 0
                    condition.getFullDescription()
                }

                helpers.ConditionTypes.Incapacitated ->
                    condition.getFullDescription()

                helpers.ConditionTypes.Invisible -> {
                    advantage = true
                    condition.getFullDescription()
                }

                helpers.ConditionTypes.Petrified -> {
                    curSpeed = 0
                    // petrified grants resistance to everything
                    resistances.addAll(helpers.DamageTypes.entries)
                    condition.getFullDescription()
                }

                helpers.ConditionTypes.Poisoned -> {
                    disadvantage = true
                    condition.getFullDescription()
                }

                helpers.ConditionTypes.Prone -> {
                    disadvantage = true
                    condition.getFullDescription()
                }

                helpers.ConditionTypes.Restrained -> {
                    curSpeed = 0
                    disadvantage = true
                    condition.getFullDescription()
                }

                helpers.ConditionTypes.Stunned -> {
                    curSpeed = 0
                    applyCondition(helpers.ConditionTypes.Incapacitated)
                    condition.getFullDescription()
                }

                helpers.ConditionTypes.Unconscious -> {
                    curSpeed = 0
                    applyCondition(helpers.ConditionTypes.Incapacitated)
                    condition.getFullDescription()
                }
            }
        }


        /**
         * Resets all derived statistics (speed, resistance, advantage/disadvantage)
         * and then reapplies every active condition from scratch.
         *
         * This ensures all current conditions stack correctly.
         */
        fun refreshDerivedStats() {
            disadvantage = false
            advantage = false
            curSpeed = speed

            // Reset resistances to defaults
            resistances = defaultResistances.toMutableSet()

            // Reapply all conditions (order matters for stacking!)
            for (cond in conditions) {
                applyCondition(cond)
            }
        }

        /**
         * Removes a single condition and reverses all effects.
         *
         * Automatically calls refreshDerivedStats() afterward to ensure
         * stacking interactions remain correct.
         *
         * @param condition Condition to remove.
         * @return A state message describing the result.
         */
        fun removeCondition(condition: helpers.ConditionTypes): String {

            if (!conditions.contains(condition)) {
                return "$name does not have $condition"
            }

            // Remove condition
            conditions.remove(condition)

            // Undo effects specific to this condition
            when (condition) {
                helpers.ConditionTypes.Paralyzed      -> curSpeed = speed
                helpers.ConditionTypes.Blinded        -> disadvantage = false
                helpers.ConditionTypes.Charmed        -> {}
                helpers.ConditionTypes.Deafened       -> {}
                helpers.ConditionTypes.Frightened     -> disadvantage = false
                helpers.ConditionTypes.Grappled       -> curSpeed = speed
                helpers.ConditionTypes.Incapacitated  -> {}
                helpers.ConditionTypes.Invisible      -> advantage = false

                helpers.ConditionTypes.Petrified -> {
                    curSpeed = speed
                    resistances = defaultResistances.toMutableSet()
                }

                helpers.ConditionTypes.Poisoned       -> disadvantage = false
                helpers.ConditionTypes.Prone          -> disadvantage = false

                helpers.ConditionTypes.Restrained -> {
                    curSpeed = speed
                    disadvantage = false
                }

                helpers.ConditionTypes.Stunned     -> curSpeed = speed
                helpers.ConditionTypes.Unconscious -> curSpeed = speed
            }

            refreshDerivedStats()
            return "$name is no longer affected by ${condition.name}"
        }

        /**
         * Removes ALL active conditions from the monster at once.
         *
         * Calls removeCondition() for each one to ensure each effect is reversed properly.
         *
         * @return A summary message.
         */
        fun removeCondition(): String {
            for (cond in conditions.toList()) {
                removeCondition(cond)
            }
            return "all conditions removed"
        }




        /**
         * Performs a saving throw roll using the given ability score.
         *
         * Includes:
         * - Advantage/disadvantage rolling
         * - Proficiency bonus if trained in this saving throw
         *
         * @param skill The ability score used for the saving throw.
         * @param adv Whether the roll has advantage.
         * @param dis Whether the roll has disadvantage.
         * @return Total saving throw result.
         */
        fun ST(skill: helpers.AbilityScores, adv: Boolean, dis: Boolean): Int {
            val roll = rollD20(adv, dis)
            val score = stat.get(skill)
            val mod = helpers.mod.getStrengthModifier(score)
            return if (saveProf.contains(skill)) roll + pb + mod else roll + mod
        }

        /**
         * Performs a skill check roll using a specific skill (e.g., Stealth, Arcana).
         *
         * Includes:
         * - Advantage/disadvantage rolling
         * - Proficiency bonus if proficient in the skill
         *
         * @param skill The skill being rolled.
         * @param adv Whether the roll has advantage.
         * @param dis Whether the roll has disadvantage.
         * @return Total skill check result.
         */
        fun SC(skill: helpers.ProficienciesSkill, adv: Boolean, dis: Boolean): Int {
            val roll = rollD20(adv, dis)
            val score = stat.get(skill.abilityType)
            val mod = helpers.mod.getStrengthModifier(score)
            return if (skillProf.contains(skill)) roll + pb + mod else roll + mod
        }

        /**
         * Rolls custom attack dice (e.g. "2d6" or "1d12").
         *
         * This method only rolls the dice.
         * It does NOT add ability modifiers or proficiency bonuses — those are handled elsewhere.
         *
         * @param attack Dice expression in XdY format.
         * @return Total of all dice rolled.
         */
        private fun toAttack(attack: String): Int {
            val parts = attack.split("d").map { it.trim() }
            val num = parts[0].toInt()
            val die = parts[1].toInt()

            var result = 0
            repeat(num) {
                result += (1..die).random()
            }
            return result
        }

        /**
         * Rolls a d20 with proper advantage/disadvantage behavior.
         *
         * @param adv Whether the roll is made at advantage.
         * @param dis Whether the roll is made at disadvantage.
         * @return The resulting d20 roll.
         */
        private fun rollD20(adv: Boolean, dis: Boolean): Int {
            val r1 = (1..20).random()
            val r2 = (1..20).random()

            return when {
                adv && dis -> r1          // cancel out — normal roll
                adv -> maxOf(r1, r2)      // take highest
                dis -> minOf(r1, r2)      // take lowest
                else -> r1                // normal roll
            }
        }

        /**
         * Wrapper for rolling an attack roll, integrating the Monster's current
         * advantage/disadvantage state unless an override is provided.
         *
         * @param override Optional: "adv" or "dis" to force advantage/disadvantage.
         * @return The resulting attack roll.
         */
        private fun rollAttack(override: String? = null): Int {
            return when (override) {
                "adv" -> rollD20(adv = true, dis = this.disadvantage)
                "dis" -> rollD20(adv = this.advantage, dis = true)
                else  -> rollD20(adv = this.advantage, dis = this.disadvantage)
            }
        }


        /**
         * Rolls an attack for this monster.
         *
         * Supports:
         * - Proficiency selection
         * - Custom attack input (e.g., "1d20 strength")
         * - Custom damage type selection
         * - Auto-parsing default monster attack strings in format: "1d20 strength fire"
         * - Advantage/disadvantage modifiers
         * - Critical hit/fail logic
         *
         * @param override "adv" for forced advantage, "dis" for forced disadvantage, or null for normal behavior.
         */
        /**
         * Performs an attack roll through UI (no console I/O).
         *
         * @param override "adv", "dis", or null
         * @return A human-readable result string for the UI log.
         */
        fun GrandRoll(override: String? = null): String {

            var attackParts: List<String> = emptyList()
            var proficient = true
            var damageType: helpers.DamageTypes = helpers.DamageTypes.Fire
            var attackStat: helpers.AbilityScores = helpers.AbilityScores.Strength
            var customAttack = ""

            // ------------------------------
            // Ask if custom attack
            // ------------------------------
            val customChoice = JOptionPane.showInputDialog(
                null,
                "Use custom attack?\n(yes/no)",
                "Attack Input",
                JOptionPane.QUESTION_MESSAGE
            )?.trim()?.lowercase() ?: "no"

            // ------------------------------
            // CUSTOM ATTACK MODE
            // ------------------------------
            if (customChoice == "yes") {

                // Ask proficiency
                val prof = JOptionPane.showInputDialog(
                    null,
                    "Is this attack proficient? (yes/no)",
                    "Proficiency",
                    JOptionPane.QUESTION_MESSAGE
                )?.trim()?.lowercase()

                proficient = prof == "yes"

                // Ask custom attack dice
                while (true) {
                    val input = JOptionPane.showInputDialog(
                        null,
                        "Enter custom attack (e.g. 1d20 strength)\nOr type 'no' to use default attack: ${this.attack}",
                        "Attack Format",
                        JOptionPane.QUESTION_MESSAGE
                    )?.trim()?.lowercase() ?: "no"

                    val parts = input.split(" ")

                    if (parts[0] == "no") {
                        customAttack = this.attack
                        break
                    }

                    val validDice = Regex("^\\d+d\\d+$").matches(parts.getOrNull(0) ?: "")
                    val validAbility = (parts.getOrNull(1) ?: "") in helpers.abilityAliases.keys

                    if (validDice && validAbility) {
                        customAttack = input
                        break
                    }

                    JOptionPane.showMessageDialog(null, "Invalid format.\nExample: 1d20 strength")
                }

                // Ask damage type
                while (true) {
                    val typeInput = JOptionPane.showInputDialog(
                        null,
                        "Enter Damage Type (fire, ice, slashing, etc.)",
                        "Damage Type",
                        JOptionPane.QUESTION_MESSAGE
                    ) ?: ""

                    val match = helpers.DamageTypes.entries.firstOrNull {
                        it.name.equals(typeInput, ignoreCase = true)
                    }

                    if (match != null) {
                        damageType = match
                        break
                    }

                    JOptionPane.showMessageDialog(null, "Invalid damage type!")
                }

                attackParts = customAttack.split(" ")
                attackStat = helpers.abilityAliases[attackParts[1]] ?: helpers.AbilityScores.Strength
            }

            // ------------------------------
            // DEFAULT MONSTER ATTACK
            // ------------------------------
            else {
                attackParts = this.attack.split(" ")

                val cleanStat = helpers.normalizeSkillInput(attackParts[1])
                attackStat = helpers.abilityAliases[cleanStat]?:helpers.AbilityScores.Strength


                damageType = helpers.DamageTypes.valueOf(
                    attackParts[2].replaceFirstChar { it.uppercase() }
                )
            }

            // ------------------------------
            // Perform attack roll
            // ------------------------------
            val attackRoll = rollAttack(override)
            var damage = toAttack(attackParts[0])
            val abilityMod = helpers.mod.getStrengthModifier(this.stat.get(attackStat))

            val resultText = StringBuilder()

            // Crit/Fails
            when (attackRoll) {
                1 -> resultText.append("CRITICAL FAIL!\n")
                20 -> {
                    resultText.append("CRITICAL HIT!\n")
                    damage += toAttack(attackParts[0])  // Double dice
                }
            }

            // Add bonuses
            damage += if (proficient) (this.pb + abilityMod) else abilityMod

            if (attackRoll == 1) {
                resultText.append("No damage dealt.")
            } else {
                resultText.append("Attack Roll: $attackRoll\n")
                resultText.append("Damage: $damage ($damageType)")
            }

            return resultText.toString()
        }


        /**
         * Performs a RAW ABILITY CHECK (Strength, Dexterity, etc.)
         *
         * @param override "adv", "dis", or null
         * @param ability The user-entered ability name (e.g., "strength", "str")
         */
        fun abilityCheck(override: String? = null, ability: String): String {

            // Roll the d20 normally/adv/dis
            val roll = rollAttack(override)

            // Normalize user input (str → strength)
            val cleaned = helpers.normalizeSkillInput(ability)

            // Convert string to enum AbilityScores
            val abilityEnum = helpers.abilityAliases[cleaned]
                ?: return("Invalid ability: '$ability'. Please enter STR/DEX/CON/INT/WIS/CHA.")

            // If user entered an invalid ability → stop

            // Get ability modifier
            val mod = helpers.mod.getStrengthModifier(this.stat.get(abilityEnum))

            // RAW ability check = roll + mod (NO PROFICIENCY)
            val total = roll + mod

            return ("Ability check (${abilityEnum.name}): $total")


        }




    }



}