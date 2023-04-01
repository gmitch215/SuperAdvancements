package me.gamercoder215.superadvancements.advancement;

import org.bukkit.entity.Player;

/**
 * A reward for an Advancement for a player upon Completion.
 */
@FunctionalInterface
public interface AReward {

    /**
     * Grant the reward to the player.
     * @param p Player granting reward to
     */
    void grant(Player p);

}
