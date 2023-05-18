package me.gamercoder215.superadvancements.wrapper;

import me.gamercoder215.superadvancements.advancement.AProgress;
import me.gamercoder215.superadvancements.advancement.Advancement;
import org.bukkit.Bukkit;
import org.bukkit.NamespacedKey;
import org.bukkit.entity.Player;

import java.util.Set;

public interface Wrapper {

    Wrapper w = getWrapper();
    
    // Implementation

    void update(Player p);

    void register(Advancement a) throws IllegalStateException;

    Advancement getAdvancement(NamespacedKey key);

    boolean isRegistered(NamespacedKey key);

    void unregister(NamespacedKey key);

    void addAdvancement(Player p, Set<Advancement> a);

    void removeAdvancement(Player p, Set<NamespacedKey> key);

    AProgress getProgress(Player p, NamespacedKey key);

    org.bukkit.advancement.Advancement toBukkit(Advancement a);

    Advancement fromBukkit(org.bukkit.advancement.Advancement a);

    // Static Util

    static String getServerVersion() {
        return Bukkit.getServer().getClass().getPackage().getName().split("\\.")[3].substring(1);
    }

    static Wrapper getWrapper() {
        String v = getServerVersion();
        try {
            return Class.forName("me.gamercoder215.superadvancements.v" + v + ".Wrapper" + v)
                .asSubclass(Wrapper.class)
                .getConstructor()
                .newInstance();
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException("Unknown Wrapper Version: " + v, e);
        }
    }

}
