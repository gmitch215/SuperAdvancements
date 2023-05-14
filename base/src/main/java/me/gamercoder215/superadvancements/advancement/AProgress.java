package me.gamercoder215.superadvancements.advancement;

import me.gamercoder215.superadvancements.advancement.criteria.ACriteriaProgress;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Map;
import java.util.stream.Collectors;

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
     * Grants the advancement to the player.
     * @return true if the Advancement was granted, false if the Advancement was already granted
     */
    boolean grant();

    /**
     * Revokes the Advancement from the player.
     * @return true if the Advancement was revoked, false if the Advancement was already revoked
     */
    boolean revoke();

    /**
     * Checks whether the Advancement progress has been completed.
     * @return true if the Advancement has been completed, false otherwise
     */
    boolean isDone();

    /**
     * Fetches all of the criteria for this AProgress.
     * @return Map of Criteria Names to Criteria
     */
    @NotNull
    Map<String, ACriteriaProgress> getCriteria();

    /**
     * Fetches an immutable copy of all of the criteria for this Advancement that has not been completed.
     * @return Map of Criteria Names to Criteria
     */
    @NotNull
    default Map<String, ACriteriaProgress> getRemainingCriteria() {
        return getCriteria().entrySet().stream()
                .filter(e -> !e.getValue().isDone())
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    }

    /**
     * Fetches an immutable copy of all of the criteria for this Advancement that has been completed.
     * @return Map of Criteria Names to Criteria
     */
    @NotNull
    default Map<String, ACriteriaProgress> getAwardedCriteria() {
        return getCriteria().entrySet().stream()
                .filter(e -> e.getValue().isDone())
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    }

    /**
     * Fetches the criteria progress for the criteria with the given name.
     * @param name Criteria Name
     * @return Criteria Progress, or null if not found
     */
    @Nullable
    default ACriteriaProgress getCriteriaProgress(@NotNull String name) {
        return getCriteria().get(name);
    }

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
    default int getTotalCriteria() {
        return getCriteria().size();
    }

    /**
     * Fetches the percentage between 0.0F and 1.0F of competed criteria.
     * @return Completed Criteria Percentage
     */
    float getPercentageCompleted();

    /**
     * Fetches the text to display on the client for this Advancement's progress.
     * @return Progress Text, or null if no more than 2 requirements
     */
    @Nullable
    String getProgressText();

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

}
