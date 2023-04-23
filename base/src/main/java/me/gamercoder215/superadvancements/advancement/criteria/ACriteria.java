package me.gamercoder215.superadvancements.advancement.criteria;

import org.jetbrains.annotations.NotNull;

/**
 * Represents an abstract Criteria object
 */
public abstract class ACriteria {

    private final ACriteriaType type;

    ACriteria(@NotNull ACriteriaType type) {
        this.type = type;
    }

    /**
     * Gets the type of the Criteria.
     * @return Criteria Type
     */
    @NotNull
    public final ACriteriaType getType() {
        return type;
    }
}
