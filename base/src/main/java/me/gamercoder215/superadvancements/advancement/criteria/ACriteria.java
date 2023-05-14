package me.gamercoder215.superadvancements.advancement.criteria;

import me.gamercoder215.superadvancements.advancement.criteria.trigger.ATrigger;
import org.jetbrains.annotations.NotNull;

import java.util.Objects;

/**
 * Represents an abstract Criteria object
 */
public final class ACriteria {

    private final ATrigger trigger;

    /**
     * Constructs a new criteria with the given trigger.
     * @param trigger Trigger for this criteria
     * @throws IllegalArgumentException if trigger is null
     */
    public ACriteria(@NotNull ATrigger trigger) throws IllegalArgumentException {
        if (trigger == null) throw new IllegalArgumentException("Trigger cannot be null");
        this.trigger = trigger;
    }

    /**
     * Fetches the trigger for this criteria.
     * @return Trigger for this criteria
     */
    @NotNull
    public ATrigger getTrigger() {
        return trigger;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ACriteria that = (ACriteria) o;
        return Objects.equals(trigger, that.trigger);
    }

    @Override
    public int hashCode() {
        return Objects.hash(trigger);
    }

}
