package me.gamercoder215.superadvancements.advancement.criteria;

import java.util.Date;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Represents progress for a specific {@link ACriteria}.
 */
public interface ACriteriaProgress {

    /**
     * Fetches the criteria this progress belongs to.
     * @return Criteria
     */
    @NotNull
    ACriteria getCriteria();
    
    /**
     * Fetches the date this criteria was obtained.
     * @return Date Obtained, or null if not obtained
     */
    @Nullable
    Date getObtained();

    /**
     * Checks whether this criteria has been obtained.
     * @return true if obtained, false otherwise
     */
    boolean isDone();

    /**
     * <p>Grants this criteria.</p>
     * <p>This method will not update automatically.</p>
     */
    void grant();

    /**
     * <p>Revokes this criteria.</p>
     * <p>This method will not update automatically.</p>
     */
    void revoke();
}
