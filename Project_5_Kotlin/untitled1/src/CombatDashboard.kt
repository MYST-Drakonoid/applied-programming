import java.awt.*
import javax.swing.*
import javax.swing.border.EmptyBorder

/**
 * Simple combat dashboard for controlling a Monster instance.
 *
 * this calss was written using the assistance of Generative AI !!PLEASE DO NOT GRADE!!
 */
class CombatDashboard(private val monster: monster_tracker.Monster) : JFrame("Monster Combat UI") {

    // -----------------------------
    // UI components (updated live)
    // -----------------------------
    private val hpLabel = JLabel()
    private val acLabel = JLabel()
    private val speedLabel = JLabel()

    private val creatureTypeLabel = JLabel("Type: ${monster.creatureType}")
    private val initiativeLabel = JLabel("Initiative: ${monster.initiative}")

    private val rollInitButton = JButton("Roll Initiative")
    private val notesButton = JButton("Show Notes")

    private val conditionsArea = JTextArea()
    private val logArea = JTextArea()

    init {
        defaultCloseOperation = EXIT_ON_CLOSE
        layout = BorderLayout()
        minimumSize = Dimension(700, 500)

        add(buildTopPanel(), BorderLayout.NORTH)
        add(buildLeftPanel(), BorderLayout.WEST)
        add(buildRightPanel(), BorderLayout.CENTER)
        add(buildLogPanel(), BorderLayout.SOUTH)

        rollInitButton.addActionListener { doInitiativeRoll() }
        notesButton.addActionListener { showOtherNotes() }

        refreshStats()
        isVisible = true
    }

    // ----------------------------------------------------------
    // TOP PANEL: Creature Type + Initiative
    // ----------------------------------------------------------
    private fun buildTopPanel(): JPanel {
        val panel = JPanel()
        panel.layout = GridLayout(2, 2, 10, 10)
        panel.border = EmptyBorder(10, 10, 10, 10)

        panel.add(creatureTypeLabel)
        panel.add(rollInitButton)
        panel.add(initiativeLabel)
        panel.add(notesButton)

        return panel
    }

    // ----------------------------------------------------------
    // LEFT PANEL (Monster Stats)
    // ----------------------------------------------------------
    private fun buildLeftPanel(): JPanel {
        val panel = JPanel()
        panel.layout = BoxLayout(panel, BoxLayout.Y_AXIS)
        panel.border = EmptyBorder(10, 10, 10, 10)
        panel.preferredSize = Dimension(250, 0)

        val nameLabel = JLabel(monster.name)
        nameLabel.font = Font("Arial", Font.BOLD, 20)

        conditionsArea.isEditable = false
        conditionsArea.lineWrap = true

        panel.add(nameLabel)
        panel.add(Box.createVerticalStrut(10))
        panel.add(hpLabel)
        panel.add(acLabel)
        panel.add(speedLabel)
        panel.add(Box.createVerticalStrut(10))
        panel.add(JLabel("Conditions:"))
        panel.add(JScrollPane(conditionsArea))

        return panel
    }

    // ----------------------------------------------------------
    // RIGHT PANEL (Actions)
    // ----------------------------------------------------------
    private fun buildRightPanel(): JPanel {
        val panel = JPanel()
        panel.layout = GridLayout(8, 1, 10, 10)
        panel.border = EmptyBorder(20, 20, 20, 20)

        val attackBtn = JButton("Roll Attack")
        attackBtn.addActionListener { doAttack() }

        val abilityBtn = JButton("Ability Check")
        abilityBtn.addActionListener { doAbilityCheck() }

        val skillBtn = JButton("Skill Check")
        skillBtn.addActionListener { doSkillCheck() }

        val dmgBtn = JButton("Apply Damage")
        dmgBtn.addActionListener { doDamage() }

        val healBtn = JButton("Heal")
        healBtn.addActionListener { doHeal() }

        val addConBtn = JButton("Apply Condition")
        addConBtn.addActionListener { doAddCondition() }

        val remConBtn = JButton("Remove Condition")
        remConBtn.addActionListener { doRemoveCondition() }

        panel.add(attackBtn)
        panel.add(abilityBtn)
        panel.add(skillBtn)
        panel.add(dmgBtn)
        panel.add(healBtn)
        panel.add(addConBtn)
        panel.add(remConBtn)

        return panel
    }

    // ----------------------------------------------------------
    // BOTTOM PANEL (Combat Log)
    // ----------------------------------------------------------
    private fun buildLogPanel(): JPanel {
        val panel = JPanel(BorderLayout())

        logArea.isEditable = false
        logArea.lineWrap = true

        val scroll = JScrollPane(logArea)
        scroll.preferredSize = Dimension(0, 150)

        panel.add(scroll, BorderLayout.CENTER)
        return panel
    }

    // ----------------------------------------------------------
    // ACTION HANDLERS
    // ----------------------------------------------------------
    private fun doAttack() {
        val result = monster.GrandRoll(null)
        appendLog(result)
        refreshStats()
    }

    private fun doAbilityCheck() {
        val ability = JOptionPane.showInputDialog(this, "Enter ability (str, dex, etc.)") ?: return
        val item = monster.abilityCheck(null, ability)
        appendLog("Ability check: $ability, Result: $item")
        refreshStats()
    }

    private fun doSkillCheck() {
        val skillInput = JOptionPane.showInputDialog(this, "Enter skill (stealth, arcana, athletics, etc.)")
            ?: return

        val cleaned = helpers.normalizeSkillInput(skillInput)
        val skillEnum = helpers.skillAliases[cleaned]

        if (skillEnum == null) {
            appendLog("Invalid skill: '$skillInput'")
            return
        }

        val advChoice = JOptionPane.showInputDialog(
            this,
            "Advantage (adv), Disadvantage (dis), or Normal?"
        )?.trim()?.lowercase()

        val adv = advChoice == "adv"
        val dis = advChoice == "dis"

        val result = monster.SC(skillEnum, adv, dis)
        appendLog("Skill check (${skillEnum.name}): $result")
        refreshStats()
    }

    private fun doDamage() {
        val amountStr = JOptionPane.showInputDialog(this, "Damage amount?") ?: return
        val amount = amountStr.toIntOrNull() ?: return

        val type = JOptionPane.showInputDialog(this, "Damage type?") ?: return
        val dType = helpers.DamageTypes.entries.firstOrNull { it.name.equals(type, true) } ?: return

        val output = monster.takeDamage(amount, dType)
        appendLog("Damage: $amount ($dType) → $output")
        refreshStats()
    }

    private fun doHeal() {
        val amountStr = JOptionPane.showInputDialog(this, "Heal amount?") ?: return
        val amount = amountStr.toIntOrNull() ?: return

        val result = monster.heal(amount)
        appendLog("Heal: $amount → $result")
        refreshStats()
    }

    private fun doAddCondition() {
        val cond = JOptionPane.showInputDialog(this, "Condition?") ?: return

        val condition = helpers.ConditionTypes.entries.firstOrNull {
            it.name.equals(cond, true)
        } ?: run {
            appendLog("Invalid condition.")
            return
        }

        appendLog("Apply Condition: ${monster.applyCondition(condition)}")
        refreshStats()
    }

    private fun doRemoveCondition() {
        val cond = JOptionPane.showInputDialog(this, "Remove which condition?") ?: return

        val condition = helpers.ConditionTypes.entries.firstOrNull {
            it.name.equals(cond, true)
        } ?: run {
            appendLog("Invalid condition.")
            return
        }

        appendLog("Remove Condition: ${monster.removeCondition(condition)}")
        refreshStats()
    }

    private fun showOtherNotes() {
        JOptionPane.showMessageDialog(
            this,
            monster.otherNotes,
            "Other Notes",
            JOptionPane.INFORMATION_MESSAGE
        )
    }

    // ----------------------------------------------------------
    // UPDATE FUNCTIONS
    // ----------------------------------------------------------
    private fun refreshStats() {
        hpLabel.text = "HP: ${monster.hp}"
        acLabel.text = "AC: ${monster.ac}"
        speedLabel.text = "Speed: ${monster.curSpeed}"

        conditionsArea.text = monster.conditions.joinToString("\n") { it.name }
        initiativeLabel.text = "Initiative: ${monster.initiative}"
    }

    private fun appendLog(text: String) {
        logArea.append("$text\n")
        logArea.caretPosition = logArea.document.length
    }

    // INITIATIVE ROLLER FIXED
    private fun doInitiativeRoll() {
        val roll = monster.ST(helpers.AbilityScores.Dexterity, false, false)
        monster.initiative = roll
        initiativeLabel.text = "Initiative: $roll"
        appendLog("Rolled initiative: $roll")
    }
}
