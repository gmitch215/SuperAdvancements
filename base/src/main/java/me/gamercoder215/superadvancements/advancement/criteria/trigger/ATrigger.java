package me.gamercoder215.superadvancements.advancement.criteria.trigger;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Iterables;
import me.gamercoder215.superadvancements.advancement.criteria.ListCriteria;
import me.gamercoder215.superadvancements.util.Range;
import org.bukkit.Keyed;
import org.bukkit.Material;
import org.bukkit.NamespacedKey;
import org.bukkit.World;
import org.bukkit.potion.PotionType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Map;
import java.util.Objects;

/**
 * <p>Represents a trigger for a {@link ListCriteria} criterion.</p>
 * <p>Null values inputted in the static methods will have those extra conditions ignored.</p>
 * <p>For more information on triggers, see <a href="https://minecraft.fandom.com/wiki/Advancement/JSON_format#Criteria">Advancement/JSON_format</a></p>
 */
public final class ATrigger implements Keyed {

    private final String name;
    private final Map<String, Object> conditions;

    private ATrigger(String name) {
        this(name, Map.of());
    }

    private ATrigger(String name, @Nullable Map<String, Object> conditions) {
        this.name = name;
        this.conditions = conditions;
    }

    // Constructor Methods

    // TODO Finish Triggers

    /**
     * Represents a trigger that is impossible to complete.
     * @return Impossible Trigger
     */
    @NotNull
    public static ATrigger impossible() {
        return new ATrigger("impossible");
    }

    /**
     * Represents a trigger that occurs when an allay drops an item on a block.
     * @return Allay Drop Item On Block Trigger
     */
    @NotNull
    public static ATrigger allayDropItemOnBlock() {
        return allayDropItemOnBlock(null, null);
    }

    /**
     * Represents a trigger that occurs when an allay drops an item on a block.
     * @param center The location predicate at the center of the block the item was dropped on.
     * @param item The item predicate for the item dropped.
     * @return Allay Drop Item On Block Trigger
     */
    @NotNull
    public static ATrigger allayDropItemOnBlock(@Nullable ATriggerPredicate.Location center, @Nullable ATriggerPredicate.Item item) {
        return new ATrigger("allay_drop_item_on_block", Map.of("location", center, "item", item));
    }

    /**
     * Represents a trigger that occurs when a warden vibration event is ignored because the source player is crouching.
     * @return Avoid Vibration Trigger
     */
    @NotNull
    public static ATrigger avoidVibration() {
        return new ATrigger("avoid_vibration");
    }

    /**
     * Represents a trigger that occurs when a player breaks a bee nest or beehive.
     * @return Bee Nest Destroyed Trigger
     */
    @NotNull
    public static ATrigger beeNestDestroyed() {
        return beeNestDestroyed(null, null, null);
    }

    /**
     * Represents a trigger that occurs when a player breaks a bee nest or beehive.
     * @param block The block that should be broken.
     * @param item The item predicate for what was used to break the block.
     * @param beesInside The number of bees that should be inside the block.
     * @return Bee Nest Destroyed Trigger
     */
    @NotNull
    public static ATrigger beeNestDestroyed(@Nullable Material block, @Nullable ATriggerPredicate.Item item, @Nullable Range beesInside) {
        return new ATrigger("bee_nest_destroyed", Map.of("block", block, "item", item, "num_bees_inside", beesInside));
    }

    /**
     * Represents a trigger that occurs when a player breeds two animals.
     * @return Bred Animals Trigger
     */
    @NotNull
    public static ATrigger bredAnimals() {
        return bredAnimals(null, null, null);
    }

    /**
     * Represents a trigger that occurs when a player breeds two animals.
     * @param child The child entity predicate.
     * @param parent The parent/partner entity predicate.
     * @return Bred Animals Trigger
     */
    @NotNull
    public static ATrigger bredAnimals(@Nullable ATriggerPredicate.Entity child, @Nullable ATriggerPredicate.Entity parent) {
        return bredAnimals(child, parent, parent);
    }

    /**
     * Represents a trigger that occurs when a player breeds two animals.
     * @param child The child entity predicate.
     * @param parent The parent entity predicate.
     * @param partner The partner entity predicate.
     * @return Bred Animals Trigger
     */
    @NotNull
    public static ATrigger bredAnimals(@Nullable ATriggerPredicate.Entity child, @Nullable ATriggerPredicate.Entity parent, @Nullable ATriggerPredicate.Entity partner) {
        return new ATrigger("bred_animals", Map.of("child", child, "parent", parent, "partner", partner));
    }

    /**
     * Represents a trigger that occurs when a player brews a potion.
     * @return Brewed Potion Trigger
     */
    @NotNull
    public static ATrigger brewedPotion() {
        return brewedPotion(null);
    }

    /**
     * Represents a trigger that occurs when a player brews a potion.
     * @param potion The potion type that should be brewed.
     * @return Brewed Potion Trigger
     */
    @NotNull
    public static ATrigger brewedPotion(@Nullable PotionType potion) {
        return new ATrigger("brewed_potion", Map.of("potion", potion));
    }

    /**
     * Represents a trigger that occurs when a player changes dimensions.
     * @return Changed Dimension Trigger
     */
    @NotNull
    public static ATrigger changedDimension() {
        return changedDimension(null, null);
    }

    /**
     * Represents a trigger that occurs when a player changes dimensions.
     * @param from The world the player is coming from.
     * @param to The world the player is going to.
     * @return Changed Dimension Trigger
     */
    @NotNull
    public static ATrigger changedDimension(@Nullable World from, @Nullable World to) {
        return new ATrigger("changed_dimension", Map.of("from", from, "to", to));
    }

    /**
     * Represents a trigger that occurs when a player uses the channeled lightning enchantment to strike lightning.
     * @return Channeled Lightning Trigger
     */
    @NotNull
    public static ATrigger channeledLightning() {
        return channeledLightning((ATriggerPredicate.Entity) null);
    }

    /**
     * Represents a trigger that occurs when a player uses the channeling enchantment to strike lightning.
     * @param victims The entities that should be struck by lightning.
     * @return Channeled Lightning Trigger
     */
    @NotNull
    public static ATrigger channeledLightning(@Nullable Iterable<? extends ATriggerPredicate.Entity> victims) {
        return new ATrigger("channeled_lightning", Map.of("victims", victims == null ? null : Iterables.toArray(victims, ATriggerPredicate.Entity.class)));
    }

    /**
     * Represents a trigger that occurs when a player uses the channeling enchantment to strike lightning.
     * @param victims The entities that should be struck by lightning.
     * @return Channeled Lightning Trigger
     */
    @NotNull
    public static ATrigger channeledLightning(@Nullable ATriggerPredicate.Entity... victims) {
        return new ATrigger("channeled_lightning", Map.of("victims", victims));
    }

    /**
     * Represents a trigger that occurs when a player changes or creates the structure of a beacon.
     * @return Construct Beacon Trigger
     */
    @NotNull
    public static ATrigger constructBeacon() {
        return constructBeacon(null);
    }

    /**
     * Represents a trigger that occurs when a player changes or creates the structure of a beacon.
     * @param level The range of the structure level of the beacon.
     * @return Construct Beacon Trigger
     */
    @NotNull
    public static ATrigger constructBeacon(@Nullable Range level) {
        return new ATrigger("construct_beacon", Map.of("level", level));
    }

    /**
     * Represents a trigger that occurs when a player consumes an item.
     * @return Consume Item Trigger
     */
    @NotNull
    public static ATrigger consumeItem() {
        return consumeItem(null);
    }

    /**
     * Represents a trigger that occurs when a player consumes an item.
     * @param item The item that should be consumed.
     * @return Consume Item Trigger
     */
    @NotNull
    public static ATrigger consumeItem(@Nullable ATriggerPredicate.Item item) {
        return new ATrigger("consume_item", Map.of("item", item));
    }

    // Implementation

    /**
     * Fetches this trigger's NamespacedKey.
     * @return Trigger Key
     */
    @NotNull
    public NamespacedKey getKey() {
        return NamespacedKey.minecraft(name);
    }

    /**
     * Fetches an immutable map of all of the inputted conditions for this trigger.
     * @return Map of Condition Names to Conditions
     */
    @NotNull
    public Map<String, Object> getConditions() {
        return ImmutableMap.copyOf(conditions);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ATrigger aTrigger = (ATrigger) o;
        return Objects.equals(name, aTrigger.name) && Objects.equals(conditions, aTrigger.conditions);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, conditions);
    }

    @Override
    public String toString() {
        return "ATrigger{" +
                "name='" + name + '\'' +
                ", conditions=" + conditions +
                '}';
    }
}
