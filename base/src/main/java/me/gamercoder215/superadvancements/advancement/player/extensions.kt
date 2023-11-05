package me.gamercoder215.superadvancements.advancement.player

import me.gamercoder215.superadvancements.advancement.Advancement
import org.bukkit.NamespacedKey

/**
 * Adds an advancement to this advancement manager.
 * @param advancement The advancement to add.
 */
operator fun PlayerAdvancementManager.plus(advancement: Advancement) = addAdvancement(advancement)

/**
 * Adds a list of advancements to this advancement manager.
 * @param advancements The advancements to add.
 */
operator fun PlayerAdvancementManager.plus(advancements: Iterable<Advancement>) = addAdvancement(advancements)

/**
 * Removes an advancement to this advancement manager.
 * @param advancement The advancement ID to remove.
 */
operator fun PlayerAdvancementManager.minus(advancement: NamespacedKey) = removeAdvancement(advancement)

/**
 * Removes an advancement from this advancement manager.
 * @param advancement The advancement to remove.
 */
operator fun PlayerAdvancementManager.minus(advancement: Advancement) = removeAdvancement(advancement)

/**
 * Removes a list of advancements from this advancement manager.
 * @param advancements The advancement IDs to remove.
 */
operator fun PlayerAdvancementManager.minus(advancements: Iterable<NamespacedKey>) = removeAdvancement(advancements)