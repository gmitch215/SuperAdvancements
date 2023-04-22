package me.gamercoder215.superadvancements.advancement.criteria;

import com.google.common.collect.ImmutableMap;
import me.gamercoder215.superadvancements.advancement.criteria.trigger.ATrigger;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Represents a criteria where a list of actions must be completed.
 */
public final class ListCriteria extends ACriteria {

    private final Map<String, ATrigger> triggers;

    ListCriteria(Map<String, ATrigger> triggers) {
        super(ACriteriaType.LIST);

        this.triggers = triggers;
    }

    /**
     * Fetches an immutable copy of all of the triggers for this criteria.
     * @return Map of Trigger Names to Triggers
     */
    @NotNull
    public Map<String, ATrigger> getTriggers() {
        return ImmutableMap.copyOf(triggers);
    }

    /**
     * Creates a new ListCriteria Builder.
     * @return Builder
     */
    @NotNull
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Creates a new ListCriteria from a map of triggers.
     * @param triggers Map of Trigger Names to Triggers
     * @return Constructed ListCriteria
     */
    @NotNull
    public static ListCriteria of(@NotNull Map<String, ATrigger> triggers) {
        return new ListCriteria(triggers);
    }

    /**
     * Creates a new ListCriteria from a single trigger.
     * @param name Name of the trigger
     * @param trigger Trigger Value
     * @return Constructed ListCriteria
     */
    @NotNull
    public static ListCriteria of(@NotNull String name, @NotNull ATrigger trigger) {
        return new ListCriteria(ImmutableMap.of(name, trigger));
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ListCriteria that = (ListCriteria) o;
        return Objects.equals(triggers, that.triggers);
    }

    @Override
    public int hashCode() {
        return Objects.hash(triggers);
    }

    /**
     * Represents a builder for a {@link ListCriteria}.
     */
    public static final class Builder {

        private Builder() {}

        private final Map<String, ATrigger> triggers = new HashMap<>();

        /**
         * Adds a trigger to the criteria.
         * @param name Name of the trigger
         * @param trigger Trigger
         * @return Builder
         * @throws IllegalArgumentException If the name or trigger is null
         */
        public Builder put(@NotNull String name, @NotNull ATrigger trigger) throws IllegalArgumentException {
            if (name == null) throw new IllegalArgumentException("Name cannot be null!");
            if (trigger == null) throw new IllegalArgumentException("Trigger cannot be null!");

            triggers.put(name, trigger);
            return this;
        }

        /**
         * Builds the criteria.
         * @return Criteria
         */
        public ListCriteria build() {
            return new ListCriteria(triggers);
        }
    }

}
