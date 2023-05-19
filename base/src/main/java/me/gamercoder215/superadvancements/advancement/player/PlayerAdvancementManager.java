package me.gamercoder215.superadvancements.advancement.player;

import com.google.common.collect.ImmutableList;
import me.gamercoder215.superadvancements.advancement.AProgress;
import me.gamercoder215.superadvancements.advancement.Advancement;
import org.bukkit.NamespacedKey;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.stream.Collectors;

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
     * Removes advancements from this Advancement Manager.
     * @param advancements Keys of Advancements to remove
     */
    void removeAdvancement(@NotNull Iterable<? extends NamespacedKey> advancements);

    /**
     * Removes advancements from this Advancement Manager.
     * @param advancements Keys of Advancements to remove
     */
    default void removeAdvancement(@NotNull NamespacedKey... advancements) {
        if (advancements == null || advancements.length == 0) return;
        removeAdvancement(ImmutableList.copyOf(advancements));
    }

    /**
     * Removes advancements from this Advancement Manager.
     * @param advancements Keys of Advancements to remove
     */
    default void removeAdvancement(@NotNull Advancement... advancements) {
        if (advancements == null || advancements.length == 0) return;
        removeAdvancement(ImmutableList.copyOf(advancements).stream().map(Advancement::getKey).collect(Collectors.toList()));
    }

    /**
     * Grants the Advancement to the player.
     * @param advancement Key of the Advancement to grant
     * @return true if the Advancement was granted, false if the Advancement was already granted
     */
    default boolean grant(@NotNull NamespacedKey advancement) {
        return getProgress(advancement).grant();
    }

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
    default boolean revoke(@NotNull NamespacedKey advancement) {
        return getProgress(advancement).revoke();
    }

    /**
     * Revokes the Advancement from the player.
     * @param advancement Advancement to revoke
     * @return true if the advancement was revoked, false if the advancement was already revoked
     */
    default boolean revoke(@NotNull Advancement advancement) {
        return revoke(advancement.getKey());
    }

    /**
     * Fetches the progress of the Advancement.
     * @param key Key of the Advancement
     * @return Progress of the Advancement
     */
    @NotNull
    AProgress getProgress(@NotNull NamespacedKey key);

    /**
     * Fetches the progress of the Advancement.
     * @param advancement Advancement to fetch
     * @return Progress of the Advancement
     */
    @NotNull
    default AProgress getProgress(@NotNull Advancement advancement) {
        return getProgress(advancement.getKey());
    }

    /**
     * Fetches the root advancement of the selected tab in the Advancement Screen.
     * @return Root advancement of the Selected Tab, or null if no tab is selected
     */
    @Nullable
    Advancement getSelectedTab();

    /**
     * <p>Sets the selected tab in the Advancement Screen.</p>
     * <p>This method uses the Advancement's {@linkplain Advancement#getRoot() root advancement}.
     * @param advancement Advancement to set as the selected tab
     */
    void setSelectedTab(@NotNull Advancement advancement);

}
