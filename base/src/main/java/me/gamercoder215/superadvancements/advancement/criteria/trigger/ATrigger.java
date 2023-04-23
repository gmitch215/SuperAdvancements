package me.gamercoder215.superadvancements.advancement.criteria.trigger;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterables;
import me.gamercoder215.superadvancements.advancement.criteria.ListCriteria;
import me.gamercoder215.superadvancements.util.Range;
import org.bukkit.*;
import org.bukkit.block.BlockState;
import org.bukkit.potion.PotionEffectType;
import org.bukkit.potion.PotionType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.*;
import java.util.logging.Level;

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

    /**
     * Represents a trigger that occurs when a player cures a zombie villager.
     * @return Cured Zombie Villager Trigger
     */
    @NotNull
    public static ATrigger curedZombieVillager() {
        return curedZombieVillager(null, null);
    }

    /**
     * Represents a trigger that occurs when a player cures a zombie villager.
     * @param villager The villager that should be cured.
     * @param zombie The zombie that should be cured.
     * @return Cured Zombie Villager Trigger
     */
    @NotNull
    public static ATrigger curedZombieVillager(@Nullable ATriggerPredicate.Entity villager, @Nullable ATriggerPredicate.Entity zombie) {
        return new ATrigger("cured_zombie_villager", Map.of("villager", villager, "zombie", zombie));
    }

    /**
     * Represents a trigger that occurs when a player uses enchants an item through an enchanting table.
     * @return Enchanted Item Trigger
     */
    @NotNull
    public static ATrigger enchantedItem() {
        return enchantedItem(null, null);
    }

    /**
     * Represents a trigger that occurs when a player enchants an item through an enchanting table.
     * @param item The item that should be enchanted.
     * @param levels The range of levels spent on the enchantment.
     * @return Enchanted Item Trigger
     */
    @NotNull
    public static ATrigger enchantedItem(@Nullable ATriggerPredicate.Item item, @Nullable Range levels) {
        return new ATrigger("enchanted_item", Map.of("item", item, "levels", levels));
    }

    /**
     * Represents a trigger that occurs when a player enters a block.
     * @return Enter Block Trigger
     */
    @NotNull
    public static ATrigger enterBlock() {
        return enterBlock(null, null);
    }

    /**
     * Represents a trigger that occurs once every tick for when the player's hitbox is inside a block.
     * @param block The block that the player is currently standing in.
     * @return Enter Block Trigger
     */
    @NotNull
    public static ATrigger enterBlock(@Nullable Material block) {
        return enterBlock(block, null);
    }

    /**
     * Represents a trigger that occurs once every tick for when the player's hitbox is inside a block.
     * @param block The block that the player is currently standing in.
     * @param state The state of the block that should be entered.
     * @return Enter Block Trigger
     */
    @NotNull
    public static ATrigger enterBlock(@Nullable Material block, @Nullable BlockState state) {
        return new ATrigger("enter_block", Map.of("block", block, "state", state));
    }

    /**
     * Represents a trigger that occurs after a player takes damage.
     * @return Entity Hurt Player Trigger
     */
    @NotNull
    public static ATrigger entityHurtPlayer() {
        return entityHurtPlayer(null);
    }

    /**
     * Represents a trigger that occurs after a player takes damage.
     * @param damage The damage predicate for the type of damage.
     * @return Entity Hurt Player Trigger
     */
    @NotNull
    public static ATrigger entityHurtPlayer(@Nullable ATriggerPredicate.Damage damage) {
        return new ATrigger("entity_hurt_player", Map.of("damage", damage));
    }

    /**
     * Represents a trigger that occurs when a player dies to a living entity.
     * @return Entity Killed Player Trigger
     */
    @NotNull
    public static ATrigger entityKilledPlayer() {
        return entityKilledPlayer(null, null);
    }

    /**
     * Represents a trigger that occurs when a player dies to a living entity.
     * @param entity The entity that should kill the player.
     * @param killingBlow The damage predicate for the type of damage the player was killed by.
     * @return Entity Killed Player Trigger
     */
    @NotNull
    public static ATrigger entityKilledPlayer(@Nullable ATriggerPredicate.Entity entity, @Nullable ATriggerPredicate.Damage killingBlow) {
        return new ATrigger("entity_killed_player", Map.of("entity", entity, "killing_blow", killingBlow));
    }

    /**
     * Represents a trigger that occurs when a player lands after falling.
     * @return Fall From Height Trigger
     */
    @NotNull
    public static ATrigger fallFromHeight() {
        return fallFromHeight(null, null);
    }


    /**
     * Represents a trigger that occurs when a player lands after falling.
     * @param startPosition The location that the player should start falling from.
     * @param distance The range of the distance the player should fall.
     * @return Fall From Height Trigger
     */
    @NotNull
    public static ATrigger fallFromHeight(@Nullable ATriggerPredicate.Location startPosition, @Nullable Range distance) {
        return new ATrigger("fall_from_height", Map.of("start_position", startPosition, "distance", distance));
    }

    /**
     * Represents a trigger that occurs when a player successfully catches an item with a fishing rod or pulls an entity with it.
     * @return Fishing Rod Hooked Trigger
     */
    @NotNull
    public static ATrigger fishingRodHooked() {
        return fishingRodHooked(null, null, null);
    }

    /**
     * Represents a trigger that occurs when a player successfully catches an item with a fishing rod or pulls an entity with it.
     * @param entity The entity that should be caught, or the fishing bobber if no entity is pulled.
     * @param item The item that should be caught.
     * @param rod The fishing rod that should be used.
     * @return Fishing Rod Hooked Trigger
     */
    @NotNull
    public static ATrigger fishingRodHooked(@Nullable ATriggerPredicate.Entity entity, @Nullable ATriggerPredicate.Item item, @Nullable ATriggerPredicate.Item rod) {
        return new ATrigger("fishing_rod_hooked", Map.of("entity", entity, "item", item, "rod", rod));
    }

    /**
     * Represents a trigger that occurs when a raid ends in a victory and the player has attacked at least one raider from that raid.
     * @return Hero Of The Village Trigger
     */
    @NotNull
    public static ATrigger heroOfTheVillage() {
        return new ATrigger("hero_of_the_village", null);
    }

    /**
     * Represents a trigger that occurs when a player's inventory changes.
     * @return Inventory Changed Trigger
     */
    @NotNull
    public static ATrigger inventoryChanged() {
        return inventoryChanged(null, null, null, (Iterable<ATriggerPredicate.Item>) null);
    }

    /**
     * Represents a trigger that occurs when a player's inventory changes.
     * @param emptySlots The range of empty slots in the player's inventory.
     * @param fullSlots The range of full slots in the player's inventory.
     * @param occupiedSlots The range of occupied slots in the player's inventory.
     * @param items The items that should be in the player's inventory.
     * @return Inventory Changed Trigger
     */
    @NotNull
    public static ATrigger inventoryChanged(@Nullable Range emptySlots, @Nullable Range fullSlots, @Nullable Range occupiedSlots, @Nullable ATriggerPredicate.Item... items) {
        return inventoryChanged(emptySlots, fullSlots, occupiedSlots, Arrays.asList(items));
    }

    /**
     * Represents a trigger that occurs when a player's inventory changes.
     * @param emptySlots The range of empty slots in the player's inventory.
     * @param fullSlots The range of full slots in the player's inventory.
     * @param occupiedSlots The range of occupied slots in the player's inventory.
     * @param items The items that should be in the player's inventory.
     * @return Inventory Changed Trigger
     */
    @NotNull
    public static ATrigger inventoryChanged(@Nullable Range emptySlots, @Nullable Range fullSlots, @Nullable Range occupiedSlots, @Nullable Iterable<? extends ATriggerPredicate.Item> items) {
        return new ATrigger("inventory_changed", Map.of("empty_slots", emptySlots, "full_slots", fullSlots, "occupied_slots", occupiedSlots, "items", ImmutableSet.copyOf(items)));
    }

    /**
     * Represents a trigger that occurs when an item in an inventory has been damaged in any form.
     * @return Item Durability Changed Trigger
     */
    @NotNull
    public static ATrigger itemDurabilityChanged() {
        return itemDurabilityChanged(null, null);
    }

    /**
     * Represents a trigger that occurs when an item in an inventory has been damaged in any form.
     * @param delta The range of the change in durability (negative ranges indicate a decrease in durability).
     * @param durability The range of the durability of the item.
     * @return Item Durability Changed Trigger
     */
    @NotNull
    public static ATrigger itemDurabilityChanged(@Nullable Range delta, @Nullable Range durability) {
        return new ATrigger("item_durability_changed", Map.of("delta", delta, "durability", durability));
    }

    /**
     * Represents a trigger that occurs when a player uses an item on a block.
     * @return Item Used On Block Trigger
     */
    @NotNull
    public static ATrigger itemUsedOnBlock() {
        return itemUsedOnBlock(null, null);
    }

    /**
     * Represents a trigger that occurs when a player uses an item on a block.
     * @param location The location of the block.
     * @param item The item that should be used.
     * @return Item Used On Block Trigger
     */
    @NotNull
    public static ATrigger itemUsedOnBlock(@Nullable ATriggerPredicate.Location location, @Nullable ATriggerPredicate.Item item) {
        return new ATrigger("item_used_on_block", Map.of("location", location, "item", item));
    }

    /**
     * Represents a trigger that occurs when a player is the source of a mob or player killed within the range of a Sculk Catalyst.
     * @return Kill Mob Near Sculk Catalyst Trigger
     */
    @NotNull
    public static ATrigger killMobNearSculkCatalyst() {
        return killMobNearSculkCatalyst(null, null);
    }

    /**
     * Represents a trigger that occurs when a player is the source of a mob or player killed within the range of a Sculk Catalyst.
     * @param entity The entity that was killed.
     * @param killingBlow The damage predicate for the killing blow.
     * @return Kill Mob Near Sculk Catalyst Trigger
     */
    @NotNull
    public static ATrigger killMobNearSculkCatalyst(@Nullable ATriggerPredicate.Entity entity, @Nullable ATriggerPredicate.Damage killingBlow) {
        return new ATrigger("kill_mob_near_sculk_catalyst", Map.of("entity", entity, "killing_blow", killingBlow));
    }

    /**
     * Represents a trigger that occurs when a player kills a mob or player with a crossbow.
     * @return Killed By Crossbow Trigger
     */
    @NotNull
    public static ATrigger killedByCrossbow() {
        return killedByCrossbow(null, (Iterable<ATriggerPredicate.Entity>) null);
    }

    /**
     * Represents a trigger that occurs when a player kills a mob or player with a crossbow.
     * @param uniqueEntityTypes The range of how many entity TYPES were killed.
     * @param victims The entities that were killed.
     * @return Killed By Crossbow Trigger
     */
    @NotNull
    public static ATrigger killedByCrossbow(@Nullable Range uniqueEntityTypes, @Nullable ATriggerPredicate.Entity... victims) {
        return killedByCrossbow(uniqueEntityTypes, Arrays.asList(victims));
    }

    /**
     * Represents a trigger that occurs when a player kills a mob or player with a crossbow.
     * @param uniqueEntityTypes The range of how many entity TYPES were killed.
     * @param victims The entities that were killed.
     * @return Killed By Crossbow Trigger
     */
    @NotNull
    public static ATrigger killedByCrossbow(@Nullable Range uniqueEntityTypes, @Nullable Iterable<? extends ATriggerPredicate.Entity> victims) {
        return new ATrigger("killed_by_crossbow", Map.of("unique_entity_types", uniqueEntityTypes, "victims", ImmutableSet.copyOf(victims)));
    }

    /**
     * Represents a trigger that occurs when a player obtains or while the player currently has the {@link PotionEffectType#LEVITATION} potion effect.
     * @return Levitation Trigger
     */
    @NotNull
    public static ATrigger levitation() {
        return levitation(null, null);
    }

    /**
     * Represents a trigger that occurs when a player obtains or while the player currently has the {@link PotionEffectType#LEVITATION} potion effect.
     * @param distance The range of the distance between when the player first started levitating and where the player is currently.
     * @param duration The range of the duration of the potion effect.
     * @return Levitation Trigger
     */
    @NotNull
    public static ATrigger levitation(@Nullable Range distance, @Nullable Range duration) {
        return new ATrigger("levitation", Map.of("distance", distance, "duration", duration));
    }

    /**
     * Represents a trigger that occurs when a lightning bolt <strong>dissappears from the world</strong>, and only for players within a {@code 256} block radius from the strike location.
     * @return Lightning Strike Trigger
     */
    @NotNull
    public static ATrigger lightningStrike() {
        return lightningStrike(null, null);
    }

    /**
     * Represents a trigger that occurs when a lightning bolt <strong>dissappears from the world</strong>, and only for players within a {@code 256} block radius from the strike location.
     * @param lightning The lightning entity.
     * @param bystander A entity predicate for someone who is within the trigger radius.
     * @return Lightning Strike Trigger
     */
    @NotNull
    public static ATrigger lightningStrike(@Nullable ATriggerPredicate.Entity lightning, @Nullable ATriggerPredicate.Entity bystander) {
        return new ATrigger("lightning_strike", Map.of("lightning", lightning, "bystander", bystander));
    }

    /**
     * Represents a trigger that occurs every 20 ticks or 1 second.
     * @return Location Trigger
     */
    @NotNull
    public static ATrigger location() {
        return new ATrigger("location", null);
    }

    /**
     * Represents a trigger that occurs when a player travels to the Nether and then returns to the Overworld.
     * @return Nether Travel Trigger
     */
    @NotNull
    public static ATrigger netherTravel() {
        return netherTravel(null, null);
    }

    /**
     * Represents a trigger that occurs when a player travels to the Nether and then returns to the Overworld.
     * @param startPosition A location predicate for the last position before the player went into the Nether.
     * @param distance The range of the distance between where the player teleported to the Nether and the player's position when they returned.
     * @return Nether Travel Trigger
     */
    @NotNull
    public static ATrigger netherTravel(@Nullable ATriggerPredicate.Location startPosition, @Nullable Range distance) {
        return new ATrigger("nether_travel", Map.of("start_position", startPosition, "distance", distance));
    }

    /**
     * Represents a trigger that occurs when a player places a block.
     * @return Placed Block Trigger
     */
    @NotNull
    public static ATrigger placedBlock() {
        return placedBlock(null, null, null, null);
    }

    /**
     * Represents a trigger that occurs when a player places a block.
     * @param block The block that was placed.
     * @return Placed Block Trigger
     */
    @NotNull
    public static ATrigger placedBlock(@Nullable Material block) {
        return placedBlock(block, null, null, null);
    }

    /**
     * Represents a trigger that occurs when a player places a block.
     * @param block The block that was placed.
     * @param item The item predicate for the item used to place the block.
     * @param location A location predicate for the location of the block.
     * @param state A Block State mirror for the block that was placed.
     * @return Placed Block Trigger
     */
    @NotNull
    public static ATrigger placedBlock(@Nullable Material block, @Nullable ATriggerPredicate.Item item, @Nullable ATriggerPredicate.Location location, @Nullable BlockState state) {
        return new ATrigger("placed_block", Map.of("block", block, "item", item, "location", location, "state", state));
    }

    /**
     * Represents a trigger that occurs when a player generates the content of a container with a loot table set.
     * @return Player Generates Container Loot Trigger
     */
    @NotNull
    public static ATrigger playerGeneratesContainerLoot() {
        return playerGeneratesContainerLoot(null);
    }

    /**
     * Represents a trigger that occurs when a player generates the content of a container with a loot table set.
     * @param lootTable The NamespacedKey of the loot table that was used to generate the container.
     * @return Player Generates Container Loot Trigger
     */
    @NotNull
    public static ATrigger playerGeneratesContainerLoot(@Nullable NamespacedKey lootTable) {
        return new ATrigger("player_generates_container_loot", Map.of("loot_table", lootTable));
    }

    /**
     * Represents a trigger that occurs when a player kills another entity.
     * @param damage The damage predicate for the damage dealt.
     * @return Player Kills Entity Trigger
     */
    @NotNull
    public static ATrigger playerHurtEntity(@Nullable ATriggerPredicate.Damage damage) {
        return playerHurtEntity(damage, null);
    }

    /**
     * Represents a trigger that occurs when a player damages another entity.
     * @param damage The damage predicate for the damage dealt.
     * @param entity The entity predicate for the entity that was damaged.
     * @return Player Hurts Entity Trigger
     */
    @NotNull
    public static ATrigger playerHurtEntity(@Nullable ATriggerPredicate.Damage damage, @Nullable ATriggerPredicate.Entity entity) {
        return new ATrigger("player_hurt_entity", Map.of("damage", damage, "entity", entity));
    }

    /**
     * Represents a trigger that occurs when a player interacts with an entity.
     * @return Player Interacted With Entity Trigger
     */
    @NotNull
    public static ATrigger playerInteractedWithEntity() {
        return playerInteractedWithEntity(null, null);
    }

    /**
     * Represents a trigger that occurs when a player interacts with an entity.
     * @param item The item predicate for the item used to interact with the entity.
     * @param entity The entity predicate for the entity that was interacted with.
     * @return Player Interacted With Entity Trigger
     */
    @NotNull
    public static ATrigger playerInteractedWithEntity(@Nullable ATriggerPredicate.Item item, ATriggerPredicate.Entity entity) {
        return new ATrigger("player_interacted_with_entity", Map.of("item", item, "entity", entity));
    }

    /**
     * Represents a trigger that occurs when a player kills another entity.
     * @return Player Killed Entity Trigger
     */
    @NotNull
    public static ATrigger playerKilledEntity() {
        return playerKilledEntity(null, null);
    }

    /**
     * Represents a trigger that occurs when a player kills another entity.
     * @param entity The entity predicate for the entity that was killed.
     * @param killingBlow The damage predicate for the killing blow.
     * @return Player Killed Entity Trigger
     */
    @NotNull
    public static ATrigger playerKilledEntity(@Nullable ATriggerPredicate.Entity entity, @Nullable ATriggerPredicate.Damage killingBlow) {
        return new ATrigger("player_killed_entity", Map.of("entity", entity, "killing_blow", killingBlow));
    }

    /**
     * Represents a trigger that occurs when a player unlocks a recipe.
     * @return Recipe Unlocked Trigger
     */
    @NotNull
    public static ATrigger recipeUnlocked() {
        return new ATrigger("recipe_unlocked", null);
    }

    /**
     * Represents a trigger that occurs when a player rides an entity in lava.
     * @return Ride Entity In Lava Trigger
     */
    @NotNull
    public static ATrigger rideEntityInLava() {
        return rideEntityInLava(null, null);
    }

    /**
     * Represents a trigger that occurs when a player mounts an entity walking on lava and while the entity moves with them (e.g. while riding a Strider).
     * @param startPosition The location predicate for the location where the player mounted the entity.
     * @param distance The range predicate for the distance between the starting position and the player's current position.
     * @return Ride Entity In Lava Trigger
     */
    @NotNull
    public static ATrigger rideEntityInLava(@Nullable ATriggerPredicate.Location startPosition, @Nullable Range distance) {
        return new ATrigger("ride_entity_in_lava", Map.of("start_position", startPosition, "distance", distance));
    }

    /**
     * Represents a trigger that occurs when a player shoots a crossbow.
     * @return Shot Crossbow Trigger
     */
    @NotNull
    public static ATrigger shotCrossbow() {
        return shotCrossbow(null);
    }

    /**
     * Represents a trigger that occurs when a player shoots a crossbow.
     * @param item The item predicate for the item used to shoot the crossbow.
     * @return Shot Crossbow Trigger
     */
    @NotNull
    public static ATrigger shotCrossbow(@Nullable ATriggerPredicate.Item item) {
        return new ATrigger("shot_crossbow", Map.of("item", item));
    }

    /**
     * Represents a trigger that occurs when a player sleeps in a bed.
     * @return Slept In Bed Trigger
     */
    @NotNull
    public static ATrigger sleptInBed() {
        return new ATrigger("slept_in_bed", null);
    }

    /**
     * Represents a trigger that occurs when a player slides down a block.
     * @return Slide Down Block Trigger
     */
    @NotNull
    public static ATrigger slideDownBlock() {
        return slideDownBlock(null, null);
    }

    /**
     * Represents a trigger that occurs when a player slides down a block.
     * @param block The block predicate for the block that was slid down.
     * @return Slide Down Block Trigger
     */
    @NotNull
    public static ATrigger slideDownBlock(@Nullable Material block) {
        return slideDownBlock(block, null);
    }

    /**
     * Represents a trigger that occurs when a player slides down a block.
     * @param block The block predicate for the block that was slid down.
     * @param state The block state for the block state that was slid down.
     * @return Slide Down Block Trigger
     */
    @NotNull
    public static ATrigger slideDownBlock(@Nullable Material block, @Nullable BlockState state) {
        return new ATrigger("slide_down_block", Map.of("block", block, "state", state));
    }

    /**
     * Represents a trigger that occurs when a player starts riding a vehicle, or an entity starts riding a vehicle currently ridden by the player.
     * @return Started Riding Trigger
     */
    @NotNull
    public static ATrigger startedRiding() {
        return new ATrigger("started_riding", null);
    }

    /**
     * Represents a trigger that occurs when a player tames an animal.
     * @return Tame Animal Trigger
     */
    @NotNull
    public static ATrigger tameAnimal() {
        return tameAnimal(null);
    }

    /**
     * Represents a trigger that occurs when a player tames an {@link org.bukkit.entity.Animals} animal.
     * @param entity The entity predicate for the entity that was tamed.
     * @return Tame Animal Trigger
     */
    @NotNull
    public static ATrigger tameAnimal(@Nullable ATriggerPredicate.Entity entity) {
        return new ATrigger("tame_animal", Map.of("entity", entity));
    }

    /**
     * Represents a trigger that occurs when a player hits a target block.
     * @return Target Hit Trigger
     */
    @NotNull
    public static ATrigger targetHit() {
        return targetHit(null, null);
    }

    /**
     * Represents a trigger that occurs when a player hits a target block.
     * @param signalStrength The range predicate for the signal strength of the target block.
     * @return Target Hit Trigger
     */
    @NotNull
    public static ATrigger targetHit(@Nullable Range signalStrength) {
        return targetHit(signalStrength, null);
    }

    /**
     * Represents a trigger that occurs when a player hits a target block.
     * @param signalStrength The range predicate for the signal strength of the target block.
     * @param projectile The entity predicate for the projectile that hit the target block.
     * @return Target Hit Trigger
     */
    @NotNull
    public static ATrigger targetHit(@Nullable Range signalStrength, @Nullable ATriggerPredicate.Entity projectile) {
        return new ATrigger("target_hit", Map.of("signal_strength", signalStrength, "projectile", projectile));
    }

    /**
     * Represents a trigger that occurs when a player throws an item and another entity picks it up.
     * @return Thrown Item Picked Up By Entity Trigger
     */
    @NotNull
    public static ATrigger thrownItemPickedUpByEntity() {
        return thrownItemPickedUpByEntity(null, null);
    }

    /**
     * Represents a trigger that occurs when a player throws an item and another entity picks it up.
     * @param item The item predicate for the item that was thrown.
     * @param entity The entity predicate for the entity that picked up the item.
     * @return Thrown Item Picked Up By Entity Trigger
     */
    @NotNull
    public static ATrigger thrownItemPickedUpByEntity(@Nullable ATriggerPredicate.Item item, @Nullable ATriggerPredicate.Entity entity) {
        return new ATrigger("thrown_item_picked_up_by_entity", Map.of("item", item, "entity", entity));
    }

    /**
     * Represents a trigger that occurs when a player picks up an item thrown by another entity.
     * @return Thrown Item Picked Up By Player Trigger
     */
    @NotNull
    public static ATrigger thrownItemPickedUpByPlayer() {
        return thrownItemPickedUpByPlayer(null, null);
    }

    /**
     * Represents a trigger that occurs when a player picks up an item thrown by another entity.
     * @param item The item predicate for the item that was thrown.
     * @param entity The entity predicate for the entity that threw the item.
     * @return Thrown Item Picked Up By Player Trigger
     */
    @NotNull
    public static ATrigger thrownItemPickedUpByPlayer(@Nullable ATriggerPredicate.Item item, @Nullable ATriggerPredicate.Entity entity) {
        return new ATrigger("thrown_item_picked_up_by_player", Map.of("item", item, "entity", entity));
    }

    /**
     * Represents a trigger that occurs every tick (1/20 of a second).
     * @return Tick Trigger
     */
    @NotNull
    public static ATrigger tick() {
        return new ATrigger("tick", null);
    }

    /**
     * Represents a trigger that occurs when a player uses an eye of ender.
     * @return Used Ender Eye Trigger
     */
    @NotNull
    public static ATrigger usedEnderEye() {
        return usedEnderEye(null);
    }

    /**
     * Represents a trigger that occurs when a player uses an eye of ender.
     * @param distance The range predicate for the distance between the player and <strong>the stronghold</strong> the eye of ender is moving towards.
     * @return Used Ender Eye Trigger
     */
    @NotNull
    public static ATrigger usedEnderEye(@Nullable Range distance) {
        return new ATrigger("used_ender_eye", Map.of("distance", distance));
    }

    /**
     * Represents a trigger that occurs when a player uses a totem of undying.
     * @return Used Totem Trigger
     */
    @NotNull
    public static ATrigger usedTotem() {
        return usedTotem(null);
    }

    /**
     * Represents a trigger that occurs when a player uses a totem of undying.
     * @param item The item predicate for the item that was used.
     * @return Used Totem Trigger
     */
    @NotNull
    public static ATrigger usedTotem(@Nullable ATriggerPredicate.Item item) {
        return new ATrigger("used_totem", Map.of("item", item));
    }

    /**
     * Represents a trigger that occurs when a player uses an item that can be used continously (e.g. drinking bottle, bow, shields, etc.)
     * @return Using Item Trigger
     */
    @NotNull
    public static ATrigger usingItem() {
        return usingItem(null);
    }

    /**
     * Represents a trigger that occurs when a player uses an item that can be used continously (e.g. drinking bottle, bow, shields, etc.)
     * @param item The item predicate for the item that was used.
     * @return Using Item Trigger
     */
    @NotNull
    public static ATrigger usingItem(@Nullable ATriggerPredicate.Item item) {
        return new ATrigger("using_item", Map.of("item", item));
    }

    /**
     * Represents a trigger that occurs when a player trades with a villager or wandering trader.
     * @return Villager Trade Trigger
     */
    @NotNull
    public static ATrigger villagerTrade() {
        return villagerTrade(null, null);
    }

    /**
     * Represents a trigger that occurs when a player trades with a villager or wandering trader.
     * @param item The item predicate for the item that was traded.
     * @param villager The entity predicate for the villager that was traded with.
     * @return Villager Trade Trigger
     */
    @NotNull
    public static ATrigger villagerTrade(@Nullable ATriggerPredicate.Item item, @Nullable ATriggerPredicate.Entity villager) {
        return new ATrigger("villager_trade", Map.of("item", item, "villager", villager));
    }

    /**
     * Represents a trigger that occurs when a player causes a raid.
     * @return Voluntary Exile Trigger
     */
    @NotNull
    public static ATrigger voluntaryExile() {
        return new ATrigger("voluntary_exile", null);
    }

    // Values

    /**
     * An array of all available triggers without any extra conditions.
     * @return All Triggers without Conditions
     */
    @NotNull
    public static ATrigger[] values() {
        Set<ATrigger> triggers = new HashSet<>();

        try {
            for (Method m : ATrigger.class.getDeclaredMethods()) {
                if (!Modifier.isStatic(m.getModifiers())) continue;
                if (!ATrigger.class.isAssignableFrom(m.getReturnType())) continue;
                if (m.getParameterCount() > 0) continue;

                triggers.add((ATrigger) m.invoke(null));
            }
        } catch (ReflectiveOperationException e) {
            Bukkit.getLogger().log(Level.SEVERE, e.getMessage(), e);
        }

        return triggers.toArray(ATrigger[]::new);
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
