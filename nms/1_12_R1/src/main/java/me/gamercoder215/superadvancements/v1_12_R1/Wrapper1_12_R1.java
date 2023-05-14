package me.gamercoder215.superadvancements.v1_12_R1;

import me.gamercoder215.superadvancements.advancement.AProgress;
import me.gamercoder215.superadvancements.advancement.Advancement;
import me.gamercoder215.superadvancements.wrapper.Wrapper;
import org.bukkit.NamespacedKey;
import org.bukkit.entity.Player;

import java.util.Set;

public final class Wrapper1_12_R1 implements Wrapper {

    @Override
    public void update(Player p) {
        // TODO Auto-generated method stub
    }

    @Override
    public void register(Advancement a) throws IllegalStateException {
        // TODO Auto-generated method stub
    }

    @Override
    public Advancement getAdvancement(NamespacedKey key) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public boolean isRegistered(NamespacedKey key) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public void unregister(NamespacedKey key) {
        // TODO Auto-generated method stub
    }

    @Override
    public void addAdvancement(Player p, Set<Advancement> a) {
        // TODO Auto-generated method stub
    }

    @Override
    public void removeAdvancement(Player p, Set<NamespacedKey> key) {
        // TODO Auto-generated method stub
    }

    @Override
    public AProgress getProgress(Player p, NamespacedKey key) {
        // TODO Auto-generated method stub
        return null;
    }
}
