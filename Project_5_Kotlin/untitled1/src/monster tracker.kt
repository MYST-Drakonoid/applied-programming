import kotlin.random.Random



class `monster tracker` {



    class Monster(
        val name: String,
        val creatureType: String,
        val speed: Int,
        var curSpeed: Int = speed,
        val stat: helpers.Stats,
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
        val saveProf: Array<helpers.AbilityScores>) {

        fun takeDamage(amount: Int, dType: helpers.DamageTypes): String {

            var effectiveDamage = when {
                weaknesses.contains(dType) -> amount * 2
                resistances.contains(dType) -> amount / 2
                immunities.contains(dType) -> 0
                else -> amount
            }

            hp -= effectiveDamage

            if (hp <= 0) return "monster death"

            return when {
                weaknesses.contains(dType) -> "Super effective"
                resistances.contains(dType) -> "not very effective"
                immunities.contains(dType) -> "no damage"
                else -> "damage taken"
            }


        }

        fun heal(amount: Int): String {
            hp += amount
            return "healed"
        }

        fun applyCondition(condition: helpers.ConditionTypes): String {
            if (!conditions.contains(condition) && (!conImmunities.contains(condition))) {

                conditions.add(condition)
                return when (condition) {
                    helpers.ConditionTypes.Paralyzed -> {
                        curSpeed = 0
                        condition.getFullDescription()
                    }

                    helpers.ConditionTypes.Blinded -> {
                        disadvantage = true
                        condition.getFullDescription()
                    }

                    helpers.ConditionTypes.Charmed -> {
                        // Custom logic if needed
                        condition.getFullDescription()
                    }

                    helpers.ConditionTypes.Deafened -> {
                        // Custom logic if needed
                        condition.getFullDescription()
                    }

                    helpers.ConditionTypes.Frightened -> {
                        disadvantage = true
                        condition.getFullDescription()
                    }

                    helpers.ConditionTypes.Grappled -> {
                        curSpeed = 0
                        condition.getFullDescription()
                    }

                    helpers.ConditionTypes.Incapacitated -> {
                        // Custom logic if needed
                        condition.getFullDescription()
                    }

                    helpers.ConditionTypes.Invisible -> {
                        advantage = true
                        condition.getFullDescription()
                    }

                    helpers.ConditionTypes.Petrified -> {
                        curSpeed = 0
                        resistances.addAll(
                            helpers.DamageTypes.entries.toTypedArray())
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
            } else {
                return when {
                    conditions.contains(condition) -> "condition is already set"
                    conImmunities.contains(condition) -> "$name is immune to ${condition.name}"
                    else -> "condition addition failed"
                }
            }
        }


        fun refreshDerivedStats() {
            disadvantage = false
            advantage = false
            curSpeed = speed
            resistances = defaultResistances.toMutableSet()

            for (cond in conditions) {
                applyCondition(cond)
            }
        }


        fun removeCondition(condition: helpers.ConditionTypes): String {
            if (!conditions.contains(condition)) {
                return "$name does not have $condition"
            }

            // Remove condition from the list
            conditions.remove(condition)

            // Revert effects associated with the condition
            when (condition) {
                helpers.ConditionTypes.Paralyzed -> curSpeed = speed
                helpers.ConditionTypes.Blinded -> disadvantage = false
                helpers.ConditionTypes.Charmed -> {} // any custom reversal logic
                helpers.ConditionTypes.Deafened -> {} // any custom reversal logic
                helpers.ConditionTypes.Frightened -> disadvantage = false
                helpers.ConditionTypes.Grappled -> curSpeed = speed
                helpers.ConditionTypes.Incapacitated -> {} // depends on if other conditions still impose it
                helpers.ConditionTypes.Invisible -> advantage = false
                helpers.ConditionTypes.Petrified -> {
                    curSpeed = speed
                    resistances = defaultResistances.toMutableSet()
                }
                helpers.ConditionTypes.Poisoned -> disadvantage = false
                helpers.ConditionTypes.Prone -> disadvantage = false
                helpers.ConditionTypes.Restrained -> {
                    curSpeed = speed
                    disadvantage = false
                }
                helpers.ConditionTypes.Stunned -> curSpeed = speed
                helpers.ConditionTypes.Unconscious -> curSpeed = speed
            }
            refreshDerivedStats()
            return "$name is no longer affected by ${condition.name}"
        }

        fun removeCondition(): String {
            for (cond in conditions.toList()) {
                removeCondition(cond)
            }
            return "all conditions removed"
        }


        fun rollD20(adv: Boolean, dis: Boolean): Int {
            val r1 = (1..20).random()
            val r2 = (1..20).random()
            return when {
                adv -> maxOf(r1, r2)
                dis -> minOf(r1, r2)
                else -> r1
            }
        }

        fun ST(skill: helpers.AbilityScores, adv: Boolean, dis: Boolean): Int {
            val roll = rollD20(adv, dis)
            val mod = stat.get(skill)
            return if (saveProf.contains(skill)) roll + pb + mod else roll + mod
        }

        fun SC(skill: helpers.ProficienciesSkill, adv: Boolean, dis: Boolean): Int {
            val roll = rollD20(adv, dis)
            val mod = stat.get(skill.abilityType)
            return if (skillProf.contains(skill)) roll + pb + mod else roll + mod
        }





    }

}