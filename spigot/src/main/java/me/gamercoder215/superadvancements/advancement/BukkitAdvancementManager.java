package me.gamercoder215.superadvancements.advancement;

import com.google.common.collect.ImmutableSet;
import me.gamercoder215.superadvancements.advancement.player.PlayerAdvancementManager;
import org.bukkit.NamespacedKey;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static me.gamercoder215.superadvancements.wrapper.Wrapper.w;

/**
 * Represents the implementation of the PlayerAdvancementManager.
 */
public final class BukkitAdvancementManager implements PlayerAdvancementManager {

    Player player;

    BukkitAdvancementManager(Player p) {
        this.player = p;
    }

    @Override
    public void addAdvancement(@NotNull Iterable<? extends Advancement> advancements) {
        if (advancements == null) return;
        w.addAdvancement(player, ImmutableSet.copyOf(advancements));
    }

    @Override
    public void removeAdvancement(@NotNull Iterable<? extends NamespacedKey> advancements) {
        if (advancements == null) return;
        w.removeAdvancement(player, ImmutableSet.copyOf(advancements));
    }

    @Override
    @NotNull
    public Player getPlayer() {
        return player;
    }

    @Override
    public void update() {
        w.update(player);
    }

    @Override
    public @NotNull AProgress getProgress(@NotNull NamespacedKey key) {
        return w.getProgress(player, key);
    }

    // Static Util

    /**
     * <p>Registers an Advancement into the server's list of Advancements.</p>
     * <p>This method is automatically called.</p>
     * @param a Advancement to register
     * @throws IllegalStateException if the Advancement is already registered
     */
    public static void register(@NotNull Advancement a) throws IllegalStateException {
        w.register(a);
    }

    /**
     * Checks if an Advancement is registered into the server's list of Advancements.
     * @param a Advancement to check
     * @return true if advancement was registered, false otherwise
     */
    public static boolean isRegistered(@NotNull Advancement a) {
        return isRegistered(a.getKey());
    }

    /**
     * Checks if an Advancement is registered into the server's list of Advancements.
     * @param key Key of Advancement to check
     * @return true if advancement was registered, false otherwise
     */
    public static boolean isRegistered(@NotNull NamespacedKey key) {
        return w.isRegistered(key);
    }

    /**
     * Unregisters an Advancement from the server's list of Advancements.
     * @param a Advancement to unregister
     */
    public static void unregister(@NotNull Advancement a) {
        unregister(a.getKey());
    }

    /**
     * Unregisters an Advancement from the server's list of Advancements.
     * @param key Key of Advancement to unregister
     */
    public static void unregister(@NotNull NamespacedKey key) {
        w.unregister(key);
    }

    /**
     * Gets an Advancement from the server's list of Advancements.
     * @param key Key of Advancement to get
     * @return Advancement found, or null if not found
     */
    @Nullable
    public static Advancement getAdvancement(@NotNull NamespacedKey key) {
        return w.getAdvancement(key);
    }

    /**
     * Converts a SuperAdvancements Advancement to a Bukkit Advancement.
     * @param a Advancement to convert
     * @return Bukkit Advancement
     */
    @NotNull
    public static org.bukkit.advancement.Advancement toBukkit(@NotNull Advancement a) {
        return w.toBukkit(a);
    }

    /**
     * Converts a Bukkit Advancement to a SuperAdvancements Advancement.
     * @param a Bukkit Advancement to convert
     * @return SuperAdvancements Advancement
     */
    @NotNull
    public static Advancement fromBukkit(@NotNull org.bukkit.advancement.Advancement a) {
        return w.fromBukkit(a);
    }

    /**
     * Creates a new BukkitAdvancementManager.
     * @param p Player to create the manager for
     * @return BukkitAdvancementManager
     */
    @NotNull
    public static BukkitAdvancementManager of(@NotNull Player p) {
        return new BukkitAdvancementManager(p);
    }

    @Override
    public @Nullable Advancement getSelectedTab() {
        return w.getSelectedTab(player);
    }

    @Override
    public void setSelectedTab(@NotNull Advancement advancement) {
        w.setSelectedTab(player, advancement);
    }

}
