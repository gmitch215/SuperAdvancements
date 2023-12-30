package me.gamercoder215.superadvancements.advancement

import org.bukkit.NamespacedKey
import org.bukkit.entity.Player

// Advancement

/**
 * Registers this advancement.
 * @see BukkitAdvancementManager.register
 */
fun Advancement.register() = BukkitAdvancementManager.register(this)

/**
 * Unregisters this advancement.
 * @see BukkitAdvancementManager.unregister
 */
fun Advancement.unregister() = BukkitAdvancementManager.unregister(this)

/**
 * Converts this advancement to a Bukkit advancement.
 * @return The Bukkit Advancement.
 * @see BukkitAdvancementManager.toBukkit
 */
fun Advancement.toBukkit() = BukkitAdvancementManager.toBukkit(this)

/**
 * Converts this advancement to a SuperAdvancements advancement.
 * @return The SuperAdvancements [Advancement].
 * @see BukkitAdvancementManager.fromBukkit
 */
fun org.bukkit.advancement.Advancement.toSuperAdvancements() = BukkitAdvancementManager.fromBukkit(this)

// Advancement$Builder

/**
 * Builds and registers this advancement.
 * @see Advancement.Builder.build
 * @see BukkitAdvancementManager.register
 */
fun Advancement.Builder.buildAndRegister() = build().also { it.register() }

// BukkitAdvancementManager

/**
 * Adds the specified advancement to this advancement manager.
 * @param advancement The advancement to add.
 * @return this advancement manager
 */
operator fun BukkitAdvancementManager.plus(advancement: Advancement): BukkitAdvancementManager {
    addAdvancement(advancement)
    return this
}

/**
 * Adds the specified advancements to this advancement manager.
 * @param advancement The advancement to add.
 * @return this advancement manager
 */
operator fun BukkitAdvancementManager.plus(advancement: Iterable<Advancement>): BukkitAdvancementManager {
    addAdvancement(advancement)
    return this
}

/**
 * Removes the specified advancement from this advancement manager.
 * @param advancement The advancement to remove.
 * @return this advancement manager
 */
operator fun BukkitAdvancementManager.minus(advancement: Advancement) = minus(advancement.key)

/**
 * Removes the specified advancements from this advancement manager.
 * @param advancement The advancement to remove, identified by its key.
 * @return this advancement manager
 */
operator fun BukkitAdvancementManager.minus(advancement: NamespacedKey): BukkitAdvancementManager {
    removeAdvancement(advancement)
    return this
}

/**
 * Removes the specified advancements from this advancement manager.
 * @param advancement The advancement to remove, identified by their keys.
 * @return this advancement manager
 */
operator fun BukkitAdvancementManager.minus(advancement: Iterable<NamespacedKey>): BukkitAdvancementManager {
    removeAdvancement(advancement)
    return this
}

// Player

/**
 * Gets the advancement manager for this player.
 * @return The advancement manager for this player.
 * @see BukkitAdvancementManager
 */
fun Player.getAdvancementManager() = BukkitAdvancementManager(this)