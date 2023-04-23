package me.gamercoder215.superadvancements.advancement;

import me.gamercoder215.superadvancements.advancement.criteria.ACriteria;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Date;
import java.util.Map;

/**
 * Represents the progress a Player has made for a specific Advancement
 */
public interface AProgress {

    /**
     * Fetches the player this progress belongs to.
     * @return Player
     */
    @NotNull
    Player getPlayer();

    /**
     * <p>Grants the Advancement to the player.</p>
     * <p>An update will not occur automatically.</p>
     * @return true if the Advancement was granted, false if the Advancement was already granted
     */
    boolean grant();

    /**
     * <p>Revokes the Advancement from the player.</p>
     * <p>An update will not occur automatically.</p>
     * @return true if the Advancement was revoked, false if the Advancement was already revoked
     */
    boolean revoke();

    /**
     * Checks whether the Advancement progress has been completed.
     * @return true if the Advancement has been completed, false otherwise
     */
    boolean isDone();

    /**
     * Fetches an immutable copy of all of the criteria for this Advancement that has not been completed.
     * @return Map of Criteria Names to Criteria
     */
    @NotNull
    Map<String, ACriteria> getRemainingCriteria();

    /**
     * Fetches an immutable copy of all of the criteria for this Advancement that has been completed.
     * @return Map of Criteria Names to Criteria
     */
    @NotNull
    Map<String, ACriteria> getAwardedCriteria();

    /**
     * Fetches the amount of criteria that has been completed.
     * @return Criteria Progress Amount
     */
    default int getCriteriaProgress() {
        return getAwardedCriteria().size();
    }

    /**
     * Fetches the total amount of criteria for this Advancement.
     * @return Total Criteria Amount
     */
    default int getTotalCriteriaProgress() {
        return getAwardedCriteria().size() + getRemainingCriteria().size();
    }

    /**
     * Grants the criteria with the given name.
     * @param name Criteria Name
     * @return true if the criteria was granted, false if the criteria was already granted
     */
    boolean grantCriteria(@NotNull String name);

    /**
     * Revokes the criteria with the given name.
     * @param name Criteria Name
     * @return true if the criteria was revoked, false if the criteria was already revoked
     */
    boolean revokeCriteria(@NotNull String name);

    /**
     * Fetches the timestamp for the last time this progress was updated, or null if it has not been updated yet
     * @return Last Update Timestamp
     */
    @Nullable
    Date getLastUpdate();

    /**
     * Sets the timestamp for the last time this progress was updated.
     * @param timestamp Last Update Timestamp
     */
    void setLastUpdate(@Nullable Date timestamp);

}
