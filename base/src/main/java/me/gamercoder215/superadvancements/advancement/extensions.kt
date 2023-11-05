package me.gamercoder215.superadvancements.advancement

import org.bukkit.Keyed
import org.bukkit.NamespacedKey
import org.bukkit.inventory.Recipe

// AReward
/**
 * Combines this reward with another reward.
 * @param reward The reward to combine with.
 */
operator fun AReward.plus(reward: AReward) = this.clone().apply {
    this.experience += reward.experience
    this.lootTables += reward.lootTables
    this.addRecipes(reward.getRecipes())
}

/**
 * Adds experience to this reward.
 * @param experience The amount of experience to add.
 */
operator fun AReward.plus(experience: Int) = this.clone().apply { this.experience += experience }

/**
 * Adds a loot table to this reward.
 * @param lootTable The loot table to add.
 */
operator fun AReward.plus(lootTable: NamespacedKey) = this.clone().apply { this.lootTables += lootTable }

/**
 * Adds a recipe to this reward.
 * @param recipe The recipe to add.
 */
operator fun <T> AReward.plus(recipe: T) where T : Recipe, T : Keyed = this.clone().apply { this.addRecipes(recipe) }

/**
 * Removes this reward with another reward.
 * @param reward The reward to combine with.
 */
operator fun AReward.minus(reward: AReward) = this.clone().apply {
    this.experience -= reward.experience
    this.lootTables -= reward.lootTables
    this.removeRecipes(reward.getRecipes())
}


/**
 * Removes experience from this reward.
 * @param experience The amount of experience to add.
 */
operator fun AReward.minus(experience: Int) = this.clone().apply { this.experience += experience }

/**
 * Removes a loot table from this reward.
 * @param lootTable The loot table to add.
 */
operator fun AReward.minus(lootTable: NamespacedKey) = this.clone().apply { this.lootTables += lootTable }

/**
 * Removes a recipe from this reward.
 * @param recipe The recipe to add.
 */
operator fun <T> AReward.minus(recipe: T) where T : Recipe, T : Keyed = this.clone().apply { this.removeRecipes(recipe) }

// Advancement$Builder

/**
 * Builds the Advancement.
 * @return Constructed Advancement
 * @throws IllegalStateException if the key or display is null, or criteria is empty
 */
@Throws(IllegalStateException::class)
fun Advancement.Builder.build(callback: (Advancement) -> Unit) = callback(build())