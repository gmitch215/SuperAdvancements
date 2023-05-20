package me.gamercoder215.superadvancements.advancement;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import me.gamercoder215.superadvancements.advancement.criteria.ACriteria;
import me.gamercoder215.superadvancements.advancement.criteria.trigger.ATrigger;
import org.bukkit.Keyed;
import org.bukkit.NamespacedKey;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

/**
 * Represents an abstract Advancement.
 */
public final class Advancement implements Keyed, Comparable<Advancement> {
    
    private final NamespacedKey key;
    private final Advancement parent;

    private ADisplay display;
    private AReward reward;
    
    private final Set<Advancement> children = new HashSet<>();
    private final Set<AFlag> flags = new HashSet<>();
    private final Map<String, ACriteria> criteria = new HashMap<>();

    private Advancement(Advancement parent, NamespacedKey key) {
        this.parent = parent;
        this.key = key;

        flags.addAll(Set.of(AFlag.MESSAGE, AFlag.TOAST));
        if (parent != null) parent.children.add(this);
    }

    /**
     * Fetches the Advancement's parent.
     * @return Advancement Parent, or null if no parent
     */
    @Nullable
    public Advancement getParent() {
        return parent;
    }

    /**
     * Fetches the Root Advancement that this Advancement is or extends from.
     * @return Root Advancement
     */
    @NotNull
    public Advancement getRoot() {
        Advancement root = this;
        while (root.getParent() != null)
            root = root.getParent();

        return root;
    }

    /**
     * Fetches the Advancement's display.
     * @return Advancement Display
     */
    @NotNull
    public ADisplay getDisplay() {
        return display;
    }

    /**
     * Sets the Advancement's display.
     * @param display Display to set
     * @throws IllegalArgumentException if display is null, or missing title, frame, or icon
     */
    public void setDisplay(@NotNull ADisplay display) throws IllegalArgumentException {
        if (display == null) throw new IllegalArgumentException("Display cannot be null");
        if (display.getTitleAsString() == null) throw new IllegalArgumentException("Display title cannot be null");
        if (display.getFrame() == null) throw new IllegalArgumentException("Display frame cannot be null");
        if (display.getIcon() == null) throw new IllegalArgumentException("Display icon cannot be null");

        this.display = display;
    }

    /**
     * Fetches the Advancement's reward.
     * @return Advancement Reward, or null if no reward
     */
    @Nullable
    public AReward getReward() {
        return reward;
    }

    /**
     * Sets the Advancement's reward.
     * @param reward Advancement Reward to set
     */
    public void setReward(@Nullable AReward reward) {
        this.reward = reward;
    }

    /**
     * Fetches an immutable set of the Advancement's children.
     * @return Advancement Children
     */
    @NotNull
    public Set<Advancement> getChildren() {
        return ImmutableSet.copyOf(children);
    }

    /**
     * Fetches an immutable set of the Advancement's flags.
     * @return Advancement Flags
     */
    @NotNull
    public Set<AFlag> getFlags() {
        return ImmutableSet.copyOf(flags);
    }

    /**
     * Sets the Advancement's flags.
     * @param flags Flags to set
     */
    public void setFlags(@Nullable Iterable<? extends AFlag> flags) {
        this.flags.clear();
        if (flags != null) this.flags.addAll(ImmutableSet.copyOf(flags));
    }

    /**
     * Sets the Advancement's flags.
     * @param flags Flags to set
     */
    public void setFlags(@Nullable AFlag... flags) {
        this.flags.clear();
        if (flags != null) setFlags(ImmutableSet.copyOf(flags));
    }

    /**
     * Fetches whether the Advancement has a flag.
     * @param flag Flag to check
     * @return true if Advancement has flag, false otherwise
     */
    public boolean hasFlag(@Nullable AFlag flag) {
        return flag != null && flags.contains(flag);
    }

    /**
     * Fetches all of the criteria for this Advancement.
     * @return Advancement Criteria
     */
    @NotNull
    public Map<String, ACriteria> getCriteria() {
        return ImmutableMap.copyOf(criteria);
    }

    /**
     * <p>Sets the Advancement's criteria.</p>
     * <p>All Advancements need to have at least one type of criteria. If you plan to award this Advancement manually, use {@link ATrigger#impossible()}.</p>
     * @param criteria Criteria to set
     * @throws IllegalArgumentException if criteria is null
     */
    public void setCriteria(@NotNull Map<String, ACriteria> criteria) throws IllegalArgumentException {
        if (criteria == null) throw new IllegalArgumentException("Criteria cannot be null");
        this.criteria.clear();
    }

    /**
     * Adds to the Advancement's criteria, setting it if it is not already set.
     * @param criteria Criteria to set
     */
    public void addCriteria(@Nullable Map<String, ACriteria> criteria) {
        if (criteria != null) criteria.forEach(this.criteria::putIfAbsent);
    }

    /**
     * Adds to the Advancement's criteria.
     * @param name Name of the criteria
     * @param criteria Criteria to set
     * @throws IllegalArgumentException if name is null
     */
    public void addCriteria(@NotNull String name, @Nullable ACriteria criteria) throws IllegalArgumentException {
        if (name == null) throw new IllegalArgumentException("Criteria name cannot be null");
        if (criteria != null) this.criteria.put(name, criteria);
    }

    /**
     * Removes a criteria from the Advancement.
     * @param name Name of the criteria
     */
    public void removeCriteria(@Nullable String name) {
        if (name != null) criteria.remove(name);
    }

    /**
     * Removes a criteria from the Advancement.
     * @param criteria Criteria to remove
     */
    public void removeCriteria(@Nullable ACriteria criteria) {
        if (criteria != null) this.criteria.values().remove(criteria);
    }

    @Override
    public NamespacedKey getKey() {
        return key;
    }

    /**
     * Fetches the Advancement Builder.
     * @return Builder
     */
    public static Builder builder() {
        return new Builder();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Advancement that = (Advancement) o;
        return Objects.equals(key, that.key);
    }

    @Override
    public int hashCode() {
        return Objects.hash(key);
    }

    @Override
    public int compareTo(@NotNull Advancement o) {
        if (o == null) throw new IllegalArgumentException("Advancement cannot be null");
        return key.toString().compareTo(o.key.toString());
    }

    /**
     * Represents the Advancement Builder.
     */
    public static final class Builder {

        NamespacedKey key;
        ADisplay display;
        Advancement parent = null;
        AReward reward;
        final Set<AFlag> flags = new HashSet<>();
        final Map<String, ACriteria> criteria = new HashMap<>();

        Builder() {}

        /**
         * Sets the NamespacedKey of the Advancement.
         * @param key Key to set
         * @return this class, for chaining
         */
        @NotNull
        public Builder key(@NotNull NamespacedKey key) {
            if (key == null) throw new IllegalArgumentException("Key cannot be null");
            this.key = key;
            return this;
        }

        /**
         * Sets the Advancement's display.
         * @param display Display to set
         * @return this class, for chaining
         */
        @NotNull
        public Builder display(@NotNull ADisplay display) {
            if (display == null) throw new IllegalArgumentException("Display cannot be null");
            this.display = display;
            return this;
        }

        /**
         * Sets the Advancement's parent advancement.
         * @param parent Parent to set
         * @return this class, for chaining
         */
        @NotNull
        public Builder parent(@Nullable Advancement parent) {
            this.parent = parent;
            return this;
        }

        /**
         * Sets the Advancement's reward.
         * @param reward Reward to set
         * @return this class, for chaining
         */
        @NotNull
        public Builder reward(@Nullable AReward reward) {
            this.reward = reward;
            return this;
        }

        /**
         * Sets the Advancement's flags.
         * @param flags Flags to set
         * @return this class, for chaining
         */
        @NotNull
        public Builder flags(@Nullable Iterable<? extends AFlag> flags) {
            this.flags.clear();
            if (flags != null) this.flags.addAll(ImmutableSet.copyOf(flags));
            return this;
        }

        /**
         * Sets the Advancement's flags.
         * @param flags Flags to set
         * @return this class, for chaining
         */
        @NotNull
        public Builder flags(@Nullable AFlag... flags) {
            if (flags != null) flags(ImmutableSet.copyOf(flags));
            return this;
        }

        /**
         * <p>Sets the Advancement's criteria.</p>
         * <p>All Advancements need to have at least one type of criteria. If you plan to award this Advancement manually, use {@link ATrigger#impossible()}.</p>
         * @param criteria Criteria to set
         * @return this class, for chaining
         */
        @NotNull
        public Builder criteria(@Nullable Map<String, ACriteria> criteria) {
            this.criteria.clear();
            if (criteria != null) this.criteria.putAll(criteria);
            return this;
        }

        /**
         * Adds to the Advancement's criteria.
         * @param name Name of the Criteria
         * @param criteria Criteria to set
         * @return this class, for chaining
         * @see #criteria(Map)
         */
        @NotNull
        public Builder criteria(@NotNull String name, @Nullable ACriteria criteria) {
            if (criteria != null) this.criteria.put(name, criteria);
            return this;
        }

        /**
         * Adds to the Advancement's criteria.
         * @param name Name of the Criteria
         * @param trigger ACriteria Trigger to set
         * @return this class, for chaining
         * @see #criteria(Map)
         */
        @NotNull
        public Builder criteria(@NotNull String name, @Nullable ATrigger trigger) {
            if (trigger != null) criteria(name, new ACriteria(trigger));
            return this;
        }

        /**
         * Builds the Advancement.
         * @return Constructed Advancement
         * @throws IllegalStateException if the key or display is null, or criteria is empty
         */
        @NotNull
        public Advancement build() throws IllegalStateException {
            if (key == null) throw new IllegalStateException("Key cannot be null");
            if (display == null) throw new IllegalStateException("Display cannot be null");
            if (criteria.isEmpty()) throw new IllegalStateException("Criteria cannot be empty");
            
            Advancement a = new Advancement(parent, key);
            a.display = display;

            if (reward != null) a.reward = reward;
            if (flags != null) a.flags.addAll(flags);
            if (criteria != null) a.criteria.putAll(criteria);

            return a;
        }

    }

}
