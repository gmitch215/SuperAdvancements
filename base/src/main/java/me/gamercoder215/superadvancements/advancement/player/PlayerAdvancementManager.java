package me.gamercoder215.superadvancements.advancement.player;

import org.bukkit.NamespacedKey;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;

import com.google.common.collect.ImmutableList;

import me.gamercoder215.superadvancements.advancement.Advancement;

/**
 * Represents a player's Advancement manager.
 */
public interface PlayerAdvancementManager {
    
    /**
     * Fetches the player this manager belongs to.
     * @return Player
     */
    @NotNull
    Player getPlayer();

    /**
     * Resends the packets involving Advancement Screen data to the player.
     */
    void update();

    /**
     * Adds advancements to this Advancement Manager.
     * @param advancements Advancements to add
     */
    void addAdvancement(@NotNull Iterable<? extends Advancement> advancements);

    /**
     * Adds advancements to this Advancement Manager.
     * @param advancements Advancements to add
     */
    default void addAdvancement(@NotNull Advancement... advancements) {
        if (advancements == null || advancements.length == 0) return;
        addAdvancement(ImmutableList.copyOf(advancements));
    }

    /**
     * Grants the Advancement to the player.
     * @param advancement Key of the Advancement to grant
     * @return true if the Advancement was granted, false if the Advancement was already granted
     */
    boolean grant(@NotNull NamespacedKey advancement);

    /**
     * Grants the Advancement to the player.
     * @param advancement Advancement to grant
     * @return true if the advancement was granted, false if the advancement was already granted
     */
    default boolean grant(@NotNull Advancement advancement) {
        return grant(advancement.getKey());
    }

    /**
     * Revokes the Advancement from the player.
     * @param advancement Key of the Advancement to revoke
     * @return true if the Advancement was revoked, false if the Advancement was already revoked
     */
    boolean revoke(@NotNull NamespacedKey advancement);

    /**
     * Revokes the Advancement from the player.
     * @param advancement Advancement to revoke
     * @return true if the advancement was revoked, false if the advancement was already revoked
     */
    default boolean revoke(@NotNull Advancement advancement) {
        return revoke(advancement.getKey());
    }

}
