package me.gamercoder215.superadvancements.advancement;

import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Set;

import org.bukkit.Bukkit;
import org.bukkit.Keyed;
import org.bukkit.NamespacedKey;
import org.bukkit.inventory.Recipe;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import com.google.common.collect.ImmutableSet;

/**
 * A reward for an Advancement for a player upon Completion.
 */
@SuppressWarnings("unchecked")
public final class AReward {

    private int experience;
    private final Set<NamespacedKey> lootTables = new HashSet<>(); 
    private final Set<NamespacedKey> recipes = new HashSet<>();

    /**
     * Creates a new Advancement Reward.
     */
    public AReward() {}

    /**
     * Creates a new Advancement Reward.
     * @param experience Experience to give the player
     */
    public AReward(int experience) {
        this(experience, null);
    }

    /**
     * Creates a new Advancement Reward.
     * @param experience Experience to give the player
     * @param lootTables Loot Tables to grant the player
     */
    public AReward(int experience, @Nullable Iterable<? extends NamespacedKey> lootTables) {
        this(experience, lootTables, null);
    }

    /**
     * Creates a new Advancement Reward.
     * @param experience Experience to give the player
     * @param lootTables Loot Tables to grant the player
     * @param recipes Recipes to grant the player
     * @throws IllegalArgumentException if experience is negative
     */
    public AReward(int experience, @Nullable Iterable<? extends NamespacedKey> lootTables, @Nullable Iterable<? extends NamespacedKey> recipes) throws IllegalArgumentException {
        if (experience < 0) throw new IllegalArgumentException("Experience cannot be negative");

        this.experience = experience;
        if (lootTables != null) this.lootTables.addAll(ImmutableSet.copyOf(lootTables));
        if (recipes != null) this.recipes.addAll(ImmutableSet.copyOf(recipes));
    }

    /**
     * Fetches the amount of experience a player will receive upon completing an advancement.
     * @return Experience Gained
     */
    public int getExperience() {
        return experience;
    }

    /**
     * Sets the amount of experience a player will receive upon completing an advancement.
     * @param experience Experience Gained
     * @throws IllegalArgumentException if experience is negative
     */
    public void setExperience(int experience) throws IllegalArgumentException {
        if (experience < 0) throw new IllegalArgumentException("Experience cannot be negative");
        this.experience = experience;
    }

    /**
     * Adds to the amount of experience a player will receive upon completing an advancement.
     * @param add Experience to add
     */
    public void addExperience(int add) {
        setExperience(getExperience() + add);
    }

    /**
     * <p>Fetches an immutable set of all of the loot tables that will be granted to the player upon completing the advancement.</p>
     * <p>Loot Tables were added in 1.13, hence using the keys of them instead.</p>
     */
    @NotNull
    public Set<NamespacedKey> getLootTables() {
        return ImmutableSet.copyOf(lootTables);
    }

    /**
     * Sets the loot tables that will be granted to the player upon completing the advancement.
     * @param lootTables Loot Tables to grant the player
     */
    public void setLootTables(@Nullable Iterable<? extends NamespacedKey> lootTables) {
        this.lootTables.clear();
        if (lootTables != null) this.lootTables.addAll(ImmutableSet.copyOf(lootTables));
    }

    /**
     * Sets the loot tables that will be granted to the player upon completing the advancement.
     * @param lootTables Loot Tables to grant the player
     */
    public void setLootTables(@Nullable NamespacedKey... lootTables) {
        setLootTables(lootTables == null ? null : ImmutableSet.copyOf(lootTables));
    }

    /**
     * Adds loot tables that will be granted to the player upon completing the advancement.
     * @param lootTables Loot Tables to grant the player
     */
    public void addLootTables(@Nullable Iterable<? extends NamespacedKey> lootTables) {
        if (lootTables != null) this.lootTables.addAll(ImmutableSet.copyOf(lootTables));
    }

    /**
     * Adds loot tables that will be granted to the player upon completing the advancement.
     * @param lootTables Loot Tables to grant the player
     */
    public void addLootTables(@Nullable NamespacedKey... lootTables) {
        addLootTables(lootTables == null ? null : ImmutableSet.copyOf(lootTables));
    }

    /**
     * Fetches an immutable set of all of the recipes that will be granted to the player upon completing the advancement.
     * @param <T> Recipe Type
     * @return Recipes
     */
    public <T extends Recipe & Keyed> Set<T> getRecipes() {
        Set<T> recipes = new HashSet<>();

        for (NamespacedKey key : this.recipes) {
            T recipe = null;

            try {
                Method getRecipe = Bukkit.class.getDeclaredMethod("getRecipe", NamespacedKey.class);
                getRecipe.setAccessible(true);
                recipe = (T) getRecipe.invoke(null, key);
            } catch (NoSuchMethodException ignored) {
            } catch (ReflectiveOperationException e) {
                throw new RuntimeException(e);
            }

            if (recipe != null) recipes.add(recipe);
        }

        return ImmutableSet.copyOf(recipes);
    }

    /**
     * Sets the AReward's recipes to the given recipes.
     * @param <T> Recipe Type
     * @param recipes Recipes to grant the player
     */
    public <T extends Recipe & Keyed> void setRecipes(@Nullable Iterable<? extends T> recipes) {
        this.recipes.clear();
        if (recipes != null) for (T recipe : recipes) this.recipes.add(recipe.getKey());
    }

    /**
     * Sets the AReward's recipes to the given recipes.
     * @param <T> Recipe Type
     * @param recipes Recipes to grant the player
     */
    @SafeVarargs
    public final <T extends Recipe & Keyed> void setRecipes(@Nullable T... recipes) {
        setRecipes(recipes == null ? null : ImmutableSet.copyOf(recipes));
    }

    /**
     * Adds recipes that will be granted to the player upon completing the advancement.
     * @param <T> Recipe Type
     * @param recipes Recipes to grant the player
     */
    public <T extends Recipe & Keyed> void addRecipes(@Nullable Iterable<? extends T> recipes) {
        if (recipes != null) for (T recipe : recipes) this.recipes.add(recipe.getKey());
    }

    /**
     * Adds recipes that will be granted to the player upon completing the advancement.
     * @param <T> Recipe Type
     * @param recipes Recipes to grant the player
     */
    @SafeVarargs
    public final <T extends Recipe & Keyed> void addRecipes(@Nullable T... recipes) {
        addRecipes(recipes == null ? null : ImmutableSet.copyOf(recipes));
    }


}
