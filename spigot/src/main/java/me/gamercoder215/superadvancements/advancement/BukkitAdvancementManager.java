package me.gamercoder215.superadvancements.advancement;

import org.bukkit.NamespacedKey;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;

import me.gamercoder215.superadvancements.advancement.player.PlayerAdvancementManager;

/**
 * Represents the Advancement Manager for Bukkit.
 */
public class BukkitAdvancementManager implements PlayerAdvancementManager {

    /**
     * Creates a new BukkitAdvancementManager.
     * @param p Player to create the manager for
     * @return BukkitAdvancementManager
     */
    public static BukkitAdvancementManager of(@NotNull Player p) {
        return new BukkitAdvancementManager(p);
    }

    Player player;

    BukkitAdvancementManager(Player p) {
        this.player = p;
    }

    @Override
    public void addAdvancement(@NotNull Iterable<? extends Advancement> advancements) {
        if (advancements == null) return;
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'addAdvancement'");
    }

    @Override
    @NotNull
    public Player getPlayer() {
        return player;
    }

    @Override
    public void update() {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'update'");
    }

    @Override
    public boolean grant(@NotNull NamespacedKey advancement) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'grant'");
    }

    @Override
    public boolean revoke(@NotNull NamespacedKey advancement) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'revoke'");
    }

}
