package me.gamercoder215.superadvancements.v1_12_R1;

import com.google.common.collect.ImmutableSet;
import me.gamercoder215.superadvancements.advancement.Advancement;
import me.gamercoder215.superadvancements.advancement.*;
import me.gamercoder215.superadvancements.advancement.criteria.ACriteria;
import me.gamercoder215.superadvancements.advancement.criteria.trigger.ATrigger;
import me.gamercoder215.superadvancements.advancement.criteria.trigger.ATriggerPredicate;
import me.gamercoder215.superadvancements.util.Range;
import me.gamercoder215.superadvancements.wrapper.Wrapper;
import net.minecraft.server.v1_12_R1.*;
import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.*;
import org.bukkit.block.Biome;
import org.bukkit.block.BlockState;
import org.bukkit.craftbukkit.v1_12_R1.CraftServer;
import org.bukkit.craftbukkit.v1_12_R1.advancement.CraftAdvancement;
import org.bukkit.craftbukkit.v1_12_R1.block.CraftBlock;
import org.bukkit.craftbukkit.v1_12_R1.enchantments.CraftEnchantment;
import org.bukkit.craftbukkit.v1_12_R1.entity.CraftPlayer;
import org.bukkit.craftbukkit.v1_12_R1.inventory.CraftItemStack;
import org.bukkit.craftbukkit.v1_12_R1.inventory.CraftShapedRecipe;
import org.bukkit.craftbukkit.v1_12_R1.inventory.CraftShapelessRecipe;
import org.bukkit.craftbukkit.v1_12_R1.potion.CraftPotionUtil;
import org.bukkit.craftbukkit.v1_12_R1.util.CraftMagicNumbers;
import org.bukkit.craftbukkit.v1_12_R1.util.CraftNamespacedKey;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.Recipe;
import org.bukkit.inventory.ShapedRecipe;
import org.bukkit.inventory.ShapelessRecipe;
import org.bukkit.potion.PotionType;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.*;
import java.util.stream.Collectors;

@SuppressWarnings({"unchecked", "deprecation"})
final class Wrapper1_12_R1 implements Wrapper {

    static {
        CraftServer server = (CraftServer) Bukkit.getServer();
        if (server != null)
            manager = server.getServer().getAdvancementData();
        else
            manager = new AdvancementDataWorld(null);
    }

    private static final AdvancementDataWorld manager;

    public static CriterionConditionValue toNMS(Range r) {
        if (r == null) return CriterionConditionValue.a;
        return new CriterionConditionValue((float) r.getMinimum(), (float) r.getMaximum());
    }

    public static Range fromNMS(CriterionConditionValue r) {
        if (r == null) return Range.ANY;

        float min = getFloat(r, "b");
        float max = getFloat(r, "c");

        return new Range(min, max);
    }

    public static EntityPlayer toNMS(Player p) {
        return ((CraftPlayer) p).getHandle();
    }

    public static MinecraftKey toNMS(NamespacedKey key) {
        if (key == null) return null;
        return CraftNamespacedKey.toMinecraft(key);
    }

    public static net.minecraft.server.v1_12_R1.ItemStack toNMS(ItemStack item) {
        if (item == null) return null;
        return CraftItemStack.asNMSCopy(item);
    }

    public static Block toNMS(Material m) {
        if (m == null) return null;
        return Block.asBlock(CraftItemStack.asNMSCopy(new ItemStack(m)).getItem());
    }

    public static net.minecraft.server.v1_12_R1.Enchantment toNMS(Enchantment enchantment) {
        return CraftEnchantment.getRaw(enchantment);
    }

    public static PotionRegistry toNMS(PotionType type) {
        if (type == null) return null;
        MinecraftKey loc = new MinecraftKey(type.name().toLowerCase());
        return PotionRegistry.a.get(loc);
    }

    public static MinecraftKey toNMS(EntityType type) {
        if (type == null) return null;
        return toNMS(NamespacedKey.minecraft(type.getName()));
    }

    public static CriterionConditionLocation toNMS(ATriggerPredicate.Location predicate) {
        if (predicate == null) return CriterionConditionLocation.a;

        DimensionManager manager = null;
        if (predicate.getDimension() != null)
            switch (predicate.getDimension().getEnvironment()) {
                case NORMAL: manager = DimensionManager.OVERWORLD; break;
                case NETHER: manager = DimensionManager.NETHER; break;
                case THE_END: manager = DimensionManager.THE_END; break;
            }

        return new CriterionConditionLocation(
                toNMS(predicate.getXRange()),
                toNMS(predicate.getYRange()),
                toNMS(predicate.getZRange()),
                predicate.getBiome() == null ? null : BiomeBase.REGISTRY_ID.get(new MinecraftKey(predicate.getBiome().name().toLowerCase())),
                "",
                manager
        );
    }

    public static CriterionConditionEnchantments toNMS(ATriggerPredicate.Enchantment predicate) {
        if (predicate == null) return CriterionConditionEnchantments.a;
        return new CriterionConditionEnchantments(
                toNMS(predicate.getEnchantment()),
                toNMS(predicate.getLevelRange())
        );
    }

    public static CriterionConditionItem toNMS(ATriggerPredicate.Item predicate) {
        if (predicate == null) return new CriterionConditionItem();

        Item item = predicate.getIncludes().stream()
                .map(Wrapper1_12_R1::toNMS)
                .map(net.minecraft.server.v1_12_R1.ItemStack::getItem)
                .findFirst()
                .orElse(null);

        CriterionConditionValue count = toNMS(predicate.getCountRange());
        CriterionConditionValue durability = toNMS(predicate.getDurabilityRange());
        CriterionConditionEnchantments[] enchants = predicate.getEnchantments()
                .stream()
                .map(Wrapper1_12_R1::toNMS)
                .toArray(CriterionConditionEnchantments[]::new);

        return new CriterionConditionItem(
                item,
                null,
                count,
                durability,
                enchants,
                null,
                CriterionConditionNBT.a
        );
    }

    public static CriterionConditionEntity toNMS(ATriggerPredicate.Entity predicate) {
        if (predicate == null) return CriterionConditionEntity.a;

        return new CriterionConditionEntity(
                toNMS(predicate.getType()),
                toNMSP(predicate.getPlayerDistanceRange()),
                toNMS(predicate.getLocation()),
                CriterionConditionMobEffect.a,
                CriterionConditionNBT.a
        );
    }

    public static CriterionConditionDamage toNMS(ATriggerPredicate.Damage predicate) {
        if (predicate == null) return new CriterionConditionDamage();

        CriterionConditionValue dealt = toNMS(predicate.getDealtRange());
        CriterionConditionValue taken = toNMS(predicate.getTakenRange());
        CriterionConditionEntity source = toNMS(predicate.getSourcePredicate());

        return new CriterionConditionDamage(
                dealt,
                taken,
                source,
                predicate.wasBlocked(),
                CriterionConditionDamageSource.a
        );
    }

    public static CriterionConditionDistance toNMSP(Range range) {
        if (range == null) return CriterionConditionDistance.a;
        return new CriterionConditionDistance(
                CriterionConditionValue.a,
                CriterionConditionValue.a,
                CriterionConditionValue.a,
                CriterionConditionValue.a,
                toNMS(range)
        );
    }

    public static CriterionInstance toNMS(ATrigger trigger) {
        if (trigger == null) return null;

        Map<String, Object> c = trigger.getConditions();
        switch (trigger.getKey().getKey()) {
            case "impossible": new CriterionTriggerImpossible.a();
            case "bred_animals": {
                ATriggerPredicate.Entity child = (ATriggerPredicate.Entity) c.get("child");
                ATriggerPredicate.Entity parent1 = (ATriggerPredicate.Entity) c.get("parent");
                ATriggerPredicate.Entity parent2 = (ATriggerPredicate.Entity) c.get("partner");

                return new CriterionTriggerBredAnimals.b(toNMS(parent1), toNMS(parent2), toNMS(child));
            }
            case "brewed_potion": {
                PotionType potion = (PotionType) c.get("potion");
                return new CriterionTriggerBrewedPotion.b(toNMS(potion));
            }
            case "changed_dimension": {
                DimensionManager from = null;
                DimensionManager to = null;

                if (c.get("from") != null)
                    switch (((World) c.get("from")).getEnvironment()) {
                        case NORMAL: from = DimensionManager.OVERWORLD; break;
                        case NETHER: from = DimensionManager.NETHER; break;
                        case THE_END: from = DimensionManager.THE_END; break;
                    }

                if (c.get("to") != null)
                    switch (((World) c.get("to")).getEnvironment()) {
                        case NORMAL: to = DimensionManager.OVERWORLD; break;
                        case NETHER: to = DimensionManager.NETHER; break;
                        case THE_END: to = DimensionManager.THE_END; break;
                    }

                return new CriterionTriggerChangedDimension.b(from, to);
            }
            case "construct_beacon": {
                Range level = (Range) c.get("level");
                return new CriterionTriggerConstructBeacon.b(toNMS(level));
            }
            case "consume_item": {
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                return new CriterionTriggerConsumeItem.b(toNMS(item));
            }
            case "cured_zombie_villager": {
                ATriggerPredicate.Entity villager = (ATriggerPredicate.Entity) c.get("villager");
                ATriggerPredicate.Entity zombie = (ATriggerPredicate.Entity) c.get("zombie");
                return new CriterionTriggerCuredZombieVillager.b(toNMS(zombie), toNMS(villager));
            }
            case "enchanted_item": {
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                Range levels = (Range) c.get("levels");
                return new CriterionTriggerEnchantedItem.b(toNMS(item), toNMS(levels));
            }
            case "enter_block": {
                Material block = (Material) c.get("block");
                BlockState state = (BlockState) c.get("state");
                return new CriterionTriggerEnterBlock.b(toNMS(block), new HashMap<>());
            }
            case "entity_hurt_player": {
                ATriggerPredicate.Damage damage = (ATriggerPredicate.Damage) c.get("damage");
                return new CriterionTriggerEntityHurtPlayer.b(toNMS(damage));
            }
            case "entity_killed_player":
            case "player_killed_entity": {
                ATriggerPredicate.Entity entity = (ATriggerPredicate.Entity) c.get("entity");
                return new CriterionTriggerKilled.b(new MinecraftKey(trigger.getKey().getKey()), toNMS(entity), CriterionConditionDamageSource.a);
            }
            case "inventory_changed": {
                Range emptySlots = (Range) c.get("empty_slots");
                Range fullSlots = (Range) c.get("full_slots");
                Range occupiedSlots = (Range) c.get("occupied_slots");
                Set<ATriggerPredicate.Item> items = (Set<ATriggerPredicate.Item>) c.get("items");

                CriterionConditionItem[] predicates = new CriterionConditionItem[0];
                if (items != null)
                    predicates = items.stream()
                            .map(Wrapper1_12_R1::toNMS)
                            .toArray(CriterionConditionItem[]::new);

                return new CriterionTriggerInventoryChanged.b(toNMS(emptySlots), toNMS(fullSlots), toNMS(occupiedSlots), predicates);
            }
            case "item_durability_changed": {
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                Range delta = (Range) c.get("delta");
                Range durability = (Range) c.get("durability");
                return new CriterionTriggerItemDurabilityChanged.b(toNMS(item), toNMS(delta), toNMS(durability));
            }
            case "levitation": {
                Range distance = (Range) c.get("distance");
                Range duration = (Range) c.get("duration");
                return new CriterionTriggerLevitation.b(toNMSP(distance), toNMS(duration));
            }
            case "location": new CriterionTriggerLocation.b(new MinecraftKey("location"), CriterionConditionLocation.a);
            case "nether_travel": {
                ATriggerPredicate.Location startPosition = (ATriggerPredicate.Location) c.get("start_position");
                Range distance = (Range) c.get("distance");
                return new CriterionTriggerNetherTravel.b(toNMS(startPosition), CriterionConditionLocation.a, toNMSP(distance));
            }
            case "placed_block": {
                Material block = (Material) c.get("block");
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                ATriggerPredicate.Location location = (ATriggerPredicate.Location) c.get("location");

                return new CriterionTriggerPlacedBlock.b(toNMS(block), new HashMap<>(), toNMS(location), toNMS(item));
            }
            case "player_hurt_entity": {
                ATriggerPredicate.Damage damage = (ATriggerPredicate.Damage) c.get("damage");
                ATriggerPredicate.Entity entity = (ATriggerPredicate.Entity) c.get("entity");
                return new CriterionTriggerPlayerHurtEntity.b(toNMS(damage), toNMS(entity));
            }
            case "recipe_unlocked": {
                NamespacedKey recipe = (NamespacedKey) c.get("recipe");

                IRecipe nms = null;
                Recipe bukkit = ImmutableSet.copyOf(Bukkit.recipeIterator())
                        .stream()
                        .filter(r -> r instanceof Keyed)
                        .map(r -> (Recipe & Keyed) r)
                        .filter(r -> r.getKey().equals(recipe))
                        .findFirst()
                        .orElse(null);

                if (bukkit != null) {
                    if (ShapedRecipe.class.isInstance(bukkit))
                        nms = getObject(CraftShapedRecipe.fromBukkitRecipe((ShapedRecipe) bukkit), "recipe", ShapedRecipes.class);
                    else if (ShapelessRecipe.class.isInstance(bukkit))
                        nms = getObject(CraftShapelessRecipe.fromBukkitRecipe((ShapelessRecipe) bukkit), "recipe", ShapelessRecipes.class);
                    else
                        throw new IllegalArgumentException("Unsupported Recipe Unlocked Type: " + bukkit.getClass().getName());
                }

                return new CriterionTriggerRecipeUnlocked.b(nms);
            }
            case "slept_in_bed": new CriterionTriggerLocation.b(new MinecraftKey("slept_in_bed"), CriterionConditionLocation.a);
            case "tame_animal": {
                ATriggerPredicate.Entity entity = (ATriggerPredicate.Entity) c.get("entity");
                return new CriterionTriggerTamedAnimal.b(toNMS(entity));
            }
            case "tick": new CriterionTriggerTick.b();
            case "used_ender_eye": {
                Range distance = (Range) c.get("distance");
                return new CriterionTriggerUsedEnderEye.b(toNMS(distance));
            }
            case "used_totem": {
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                return new CriterionTriggerUsedTotem.b(toNMS(item));
            }
            case "villager_trade": {
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                ATriggerPredicate.Entity villager = (ATriggerPredicate.Entity) c.get("villager");
                return new CriterionTriggerVillagerTrade.b(toNMS(villager), toNMS(item));
            }
            case "voluntary_exile": return new CriterionTriggerLocation.b(new MinecraftKey("voluntary_exile"), CriterionConditionLocation.a);
            default: throw new IllegalArgumentException("Unknown Advancement Trigger: " + trigger);
        }
    }


    public static float getFloat(Object o, String name) { return getObject(o, name, Float.class); }

    public static double getDouble(Object o, String name) {
        return getObject(o, name, Double.class);
    }

    public static boolean getBoolean(Object o, String name) {
        return getObject(o, name, Boolean.class);
    }

    public static int getInt(Object o, String name) {
        return getObject(o, name, Integer.class);
    }

    public static <T> T getObject(Object o, String name, Class<T> cast) {
        try {
            Class<?> clazz = o.getClass();

            while (clazz.getSuperclass() != null) {
                try {
                    Field f = clazz.getDeclaredField(name);
                    f.setAccessible(true);
                    return cast.cast(f.get(o));
                } catch (NoSuchFieldException | ClassCastException e) {
                    clazz = clazz.getSuperclass();
                }
            }
        } catch (Exception e) {
            throw new RuntimeException(e);
        }

        return null;
    }

    public static void setObject(Object o, String name, Object value) {
        try {
            Class<?> clazz = o.getClass();

            while (clazz.getSuperclass() != null) {
                try {
                    Field f = clazz.getDeclaredField(name);
                    f.setAccessible(true);
                    f.set(o, value);
                    return;
                } catch (NoSuchFieldException e) {
                    clazz = clazz.getSuperclass();
                }
            }
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public static ATrigger fromNMS(CriterionInstance t) {
        if (t == null) return null;

        switch (t.a().getKey()) {
            case "impossible": return ATrigger.impossible();
            case "allay_drop_item_on_block": return ATrigger.allayDropItemOnBlock(
                fromNMS(getObject(t, "a", CriterionConditionLocation.class)),
                fromNMS(getObject(t, "b", CriterionConditionItem.class))
            );
            case "avoid_vibration": return ATrigger.avoidVibration();
            case "bee_nest_destroyed": return ATrigger.beeNestDestroyed(
                fromNMS(getObject(t, "a", Block.class)),
                fromNMS(getObject(t, "b", CriterionConditionItem.class)),
                fromNMS(getObject(t, "c", CriterionConditionValue.class))
            );
            case "bred_animals": return ATrigger.bredAnimals(
                fromNMS(getObject(t, "c", CriterionConditionEntity.class)),
                fromNMS(getObject(t, "a", CriterionConditionEntity.class)),
                fromNMS(getObject(t, "b", CriterionConditionEntity.class))
            );
            case "brewed_potion": return ATrigger.brewedPotion(
                fromNMS(getObject(t, "a", PotionRegistry.class))
            );
            case "changed_dimension": return ATrigger.changedDimension(
                fromNMSW(getObject(t, "a", DimensionManager.class)),
                fromNMSW(getObject(t, "b", DimensionManager.class))
            );
            case "channeled_lightning": return ATrigger.channeledLightning(
                Arrays.stream(getObject(t, "a", CriterionConditionEntity[].class))
                        .map(Wrapper1_12_R1::fromNMS)
                        .collect(Collectors.toSet())
            );
            case "construct_beacon": return ATrigger.constructBeacon(
                fromNMS(getObject(t, "a", CriterionConditionValue.class))
            );
            case "consume_item": return ATrigger.consumeItem(
                fromNMS(getObject(t, "a", CriterionConditionItem.class))
            );
            case "cured_zombie_villager": return ATrigger.curedZombieVillager(
                fromNMS(getObject(t, "a", CriterionConditionEntity.class)),
                fromNMS(getObject(t, "b", CriterionConditionEntity.class))
            );
            case "enchanted_item": return ATrigger.enchantedItem(
                fromNMS(getObject(t, "a", CriterionConditionItem.class)),
                fromNMS(getObject(t, "b", CriterionConditionValue.class))
            );
            case "enter_block": return ATrigger.enterBlock(
                fromNMS(getObject(t, "a", Block.class)),
                null
            );
            case "entity_hurt_player": return ATrigger.entityHurtPlayer(
                fromNMS(getObject(t, "a", CriterionConditionDamage.class))
            );
            case "entity_killed_player": return ATrigger.entityKilledPlayer(
                fromNMS(getObject(t, "a", CriterionConditionEntity.class))
            );
            case "fall_from_height": return ATrigger.fallFromHeight(
                fromNMS(getObject(t, "b", CriterionConditionDistance.class)),
                fromNMS(getObject(t, "a", CriterionConditionLocation.class))
            );
            case "fishing_rod_hooked": return ATrigger.fishingRodHooked(
                fromNMS(getObject(t, "a", CriterionConditionItem.class)),
                fromNMS(getObject(t, "b", CriterionConditionEntity.class)),
                fromNMS(getObject(t, "c", CriterionConditionItem.class))
            );
            case "hero_of_the_village": return ATrigger.heroOfTheVillage();
            case "inventory_changed": return ATrigger.inventoryChanged(
                fromNMS(getObject(t, "c", CriterionConditionValue.class)),
                fromNMS(getObject(t, "b", CriterionConditionValue.class)),
                fromNMS(getObject(t, "a", CriterionConditionValue.class)),
                Arrays.stream(getObject(t, "d", CriterionConditionItem[].class))
                        .map(Wrapper1_12_R1::fromNMS)
                        .collect(Collectors.toSet())
            );
            case "item_durability_changed": return ATrigger.itemDurabilityChanged(
                fromNMS(getObject(t, "a", CriterionConditionItem.class)),
                fromNMS(getObject(t, "c", CriterionConditionValue.class)),
                fromNMS(getObject(t, "b", CriterionConditionValue.class))
            );
            case "item_used_on_block": return ATrigger.itemUsedOnBlock(
                fromNMS(getObject(t, "a", CriterionConditionLocation.class)),
                fromNMS(getObject(t, "b", CriterionConditionItem.class))
            );
            case "kill_mob_near_sculk_catalyst": return ATrigger.killMobNearSculkCatalyst(
                fromNMS(getObject(t, "a", CriterionConditionEntity.class))
            );
            case "levitation": return ATrigger.levitation(
                fromNMS(getObject(t, "a", CriterionConditionDistance.class)),
                fromNMS(getObject(t, "b", CriterionConditionValue.class))
            );
            case "location": return ATrigger.location();
            case "nether_travel": return ATrigger.netherTravel(
                fromNMS(getObject(t, "a", CriterionConditionLocation.class)),
                fromNMS(getObject(t, "b", CriterionConditionDistance.class))
            );
            case "placed_block": return ATrigger.placedBlock(
                fromNMS(getObject(t, "a", Block.class)),
                fromNMS(getObject(t, "d", CriterionConditionItem.class)),
                fromNMS(getObject(t, "c", CriterionConditionLocation.class)),
                null
            );
            case "player_generates_container_loot": return ATrigger.playerGeneratesContainerLoot(
                fromNMS(getObject(t, "a", MinecraftKey.class))
            );
            case "player_hurt_entity": return ATrigger.playerHurtEntity(
                fromNMS(getObject(t, "a", CriterionConditionDamage.class)),
                fromNMS(getObject(t, "b", CriterionConditionEntity.class))
            );
            case "player_interacted_with_entity": return ATrigger.playerInteractedWithEntity(
                fromNMS(getObject(t, "b", CriterionConditionEntity.class)),
                fromNMS(getObject(t, "a", CriterionConditionItem.class))
            );
            case "player_killed_entity": return ATrigger.playerKilledEntity(
                fromNMS(getObject(t, "a", CriterionConditionEntity.class))
            );
            case "recipe_unlocked": return ATrigger.recipeUnlocked(
                fromNMS(getObject(t, "a", MinecraftKey.class))
            );
            case "ride_entity_in_lava": return ATrigger.rideEntityInLava(
                fromNMS(getObject(t, "a", CriterionConditionLocation.class)),
                fromNMS(getObject(t, "b", CriterionConditionDistance.class))
            );
            case "slept_in_bed": return ATrigger.sleptInBed();
            case "started_riding": return ATrigger.startedRiding();
            case "tame_animal": return ATrigger.tameAnimal(
                fromNMS(getObject(t, "a", CriterionConditionEntity.class))
            );
            case "target_hit": return ATrigger.targetHit(
                fromNMS(getObject(t, "a", CriterionConditionValue.class)),
                fromNMS(getObject(t, "b", CriterionConditionEntity.class))
            );
            case "thrown_item_picked_up_by_entity": return ATrigger.thrownItemPickedUpByEntity(
                fromNMS(getObject(t, "a", CriterionConditionItem.class)),
                fromNMS(getObject(t, "b", CriterionConditionEntity.class))
            );
            case "thrown_item_picked_up_by_player": return ATrigger.thrownItemPickedUpByPlayer(
                fromNMS(getObject(t, "a", CriterionConditionItem.class)),
                fromNMS(getObject(t, "b", CriterionConditionEntity.class))
            );
            case "tick": return ATrigger.tick();
            case "used_ender_eye": return ATrigger.usedEnderEye(
                fromNMS(getObject(t, "a", CriterionConditionValue.class))
            );
            case "used_totem": return ATrigger.usedTotem(
                fromNMS(getObject(t, "a", CriterionConditionItem.class))
            );
            case "using_item": return ATrigger.usingItem(
                fromNMS(getObject(t, "a", CriterionConditionItem.class))
            );
            case "villager_trade": return ATrigger.villagerTrade(
                fromNMS(getObject(t, "b", CriterionConditionItem.class)),
                fromNMS(getObject(t, "a", CriterionConditionEntity.class))
            );
            case "voluntary_exile": return ATrigger.voluntaryExile();
            default: throw new IllegalArgumentException("Unknown Advancement Trigger: " + t.a());
        }
    }

    public static Criterion toNMS(ACriteria criteria) {
        if (criteria == null) return null;
        return new Criterion(toNMS(criteria.getTrigger()));
    }

    public static AdvancementRewards toNMS(AReward reward) {
        if (reward == null) return AdvancementRewards.a;
        return new AdvancementRewards(
                reward.getExperience(),
                reward.getLootTables() == null ? null : reward.getLootTables().stream()
                        .map(Wrapper1_12_R1::toNMS)
                        .toArray(MinecraftKey[]::new),
                reward.getRecipes() == null ? null : reward.getRecipes().stream()
                        .map(Keyed::getKey)
                        .map(Wrapper1_12_R1::toNMS)
                        .toArray(MinecraftKey[]::new),
                CustomFunction.a.a
        );
    }

    public static net.minecraft.server.v1_12_R1.Advancement toNMS(Advancement a) {
        if (a == null) return null;
        if (AdvancementDataWorld.REGISTRY.advancements.containsKey(toNMS(a.getKey()))) return manager.a(toNMS(a.getKey()));

        ADisplay display = a.getDisplay();
        String title = display.getTitleAsString();
        String desc = display.getDescriptionAsString();
        AdvancementFrameType frame = Arrays.stream(AdvancementFrameType.values()).filter(f -> f.a().equalsIgnoreCase(display.getFrame().name())).findFirst().orElse(AdvancementFrameType.TASK);
        MinecraftKey bg = null;

        if (a.getParent() == null && display.getBackgroundTexture() != null)
            bg = new MinecraftKey(display.getBackgroundTexture());

        AdvancementDisplay nmsDisplay = new AdvancementDisplay(toNMS(display.getIcon()), new ChatComponentText(title), new ChatComponentText(desc), bg, frame, a.hasFlag(AFlag.TOAST), a.hasFlag(AFlag.MESSAGE), a.hasFlag(AFlag.HIDDEN));
        nmsDisplay.a(display.getX(), display.getY());

        net.minecraft.server.v1_12_R1.Advancement parent = a.getParent() == null ? null : toNMS(a.getParent());

        return new net.minecraft.server.v1_12_R1.Advancement(
                toNMS(a.getKey()),
                parent,
                nmsDisplay,
                toNMS(a.getReward()),
                a.getCriteria().entrySet().stream()
                        .collect(Collectors.toMap(Map.Entry::getKey, e -> toNMS(e.getValue()))),
                a.hasFlag(AFlag.MERGE_CRITERIA) ? AdvancementRequirements1_12_R1.OR.createRequirements(a.getCriteria().keySet()) : AdvancementRequirements1_12_R1.AND.createRequirements(a.getCriteria().keySet())
        );
    }

    public static ItemStack fromNMS(net.minecraft.server.v1_12_R1.ItemStack item) {
        if (item == null) return null;
        return CraftItemStack.asBukkitCopy(item);
    }

    public static NamespacedKey fromNMS(MinecraftKey key) {
        if (key == null) return null;
        return CraftNamespacedKey.fromMinecraft(key);
    }

    public static World fromNMSW(DimensionManager world) {
        if (world == null) return null;
        return Bukkit.getWorlds().get(world.getDimensionID());
    }

    public static Material fromNMS(Block block) {
        if (block == null) return null;
        return CraftMagicNumbers.getMaterial(block);
    }

    public static Range fromNMS(CriterionConditionDistance p) {
        if (p == null) return null;
        return fromNMS(getObject(p, "f", CriterionConditionValue.class));
    }

    public static ItemStack fromNMS(Item item) {
        if (item == null) return null;
        return CraftItemStack.asBukkitCopy(new net.minecraft.server.v1_12_R1.ItemStack(item));
    }

    public static Enchantment fromNMS(net.minecraft.server.v1_12_R1.Enchantment enchant) {
        if (enchant == null) return null;
        int id = net.minecraft.server.v1_12_R1.Enchantment.getId(enchant);
        return CraftEnchantment.getById(id);
    }

    public static PotionType fromNMS(PotionRegistry potion) {
        if (potion == null) return null;
        MinecraftKey loc = PotionRegistry.a.b(potion);
        return CraftPotionUtil.toBukkit(loc.getKey()).getType();
    }

    public static Biome fromNMS(BiomeBase base) {
        if (base == null) return null;
        return CraftBlock.biomeBaseToBiome(base);
    }

    public static ATriggerPredicate.Damage fromNMS(CriterionConditionDamage p) {
        if (p == null) return null;
        return ATriggerPredicate.Damage.builder()
                .dealt(fromNMS(getObject(p, "b", CriterionConditionValue.class)))
                .taken(fromNMS(getObject(p, "c", CriterionConditionValue.class)))
                .source(fromNMS(getObject(p, "d", CriterionConditionEntity.class)))
                .build();
    }

    public static ATriggerPredicate.Entity fromNMS(CriterionConditionEntity p) {
        if (p == null) return null;

        return ATriggerPredicate.Entity.builder()
                .distanceToPlayer(fromNMS(getObject(p, "c", CriterionConditionDistance.class)))
                .location(fromNMS(getObject(p, "d", CriterionConditionLocation.class)))
                .steppingLocation(fromNMS(getObject(p, "e", CriterionConditionLocation.class)))
                .build();
    }

    public static ATriggerPredicate.Enchantment fromNMS(CriterionConditionEnchantments p) {
        if (p == null) return null;
        return ATriggerPredicate.Enchantment.builder()
                .enchantment(fromNMS(getObject(p, "c", net.minecraft.server.v1_12_R1.Enchantment.class)))
                .level(fromNMS(getObject(p, "d", CriterionConditionValue.class)))
                .build();
    }

    public static ATriggerPredicate.Item fromNMS(CriterionConditionItem p) {
        if (p == null) return null;
        ATriggerPredicate.Item.Builder builder = ATriggerPredicate.Item.builder()
                .include(((Set<Item>) getObject(p, "c", Set.class))
                        .stream()
                        .map(Wrapper1_12_R1::fromNMS)
                        .collect(Collectors.toSet())
                )
                .count(fromNMS(getObject(p, "d", CriterionConditionValue.class)))
                .durability(fromNMS(getObject(p, "e", CriterionConditionValue.class)));

        for (CriterionConditionEnchantments ench : getObject(p, "f", CriterionConditionEnchantments[].class))
            builder.enchantment(fromNMS(ench));
        
        for (CriterionConditionEnchantments ench : getObject(p, "g", CriterionConditionEnchantments[].class))
            builder.storedEnchantment(fromNMS(ench));

        return builder.build();
    }

    public static ATriggerPredicate.Location fromNMS(CriterionConditionLocation p) {
        if (p == null) return null;
        return ATriggerPredicate.Location.builder()
                .x(fromNMS(getObject(p, "b", CriterionConditionValue.class)))
                .y(fromNMS(getObject(p, "c", CriterionConditionValue.class)))
                .z(fromNMS(getObject(p, "d", CriterionConditionValue.class)))
                .biome(fromNMS(getObject(p, "e", BiomeBase.class)))
                .dimension(fromNMSW(getObject(p, "g", DimensionManager.class)))
                .smokey(getBoolean(p, "i"))
                .build();
    }

    public static ACriteria fromNMS(Criterion c) {
        if (c == null) return null;
        return new ACriteria(fromNMS(c.a()));
    }

    public static AReward fromNMS(AdvancementRewards rewards) {
        if (rewards == null) return AReward.EMPTY;
        try {
            Field experienceF = AdvancementRewards.class.getDeclaredField("b");
            experienceF.setAccessible(true);
            int experience = experienceF.getInt(rewards);

            Field lootF = AdvancementRewards.class.getDeclaredField("c");
            lootF.setAccessible(true);
            MinecraftKey[] loot = (MinecraftKey[]) lootF.get(rewards);

            Field recipesF = AdvancementRewards.class.getDeclaredField("d");
            recipesF.setAccessible(true);
            MinecraftKey[] recipes = (MinecraftKey[]) recipesF.get(rewards);

            return new AReward(
                    experience,
                    Arrays.stream(loot).map(Wrapper1_12_R1::fromNMS).collect(Collectors.toList()),
                    Arrays.stream(recipes).map(Wrapper1_12_R1::fromNMS).collect(Collectors.toList())
            );
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }
    }

    public static Advancement fromNMS(net.minecraft.server.v1_12_R1.Advancement a) {
        if (a == null) return null;
        NMSDisplay1_12_R1 display = new NMSDisplay1_12_R1(a.c());

        Map<String, ACriteria> criteria = a.getCriteria()
                .entrySet()
                .stream()
                .collect(Collectors.toMap(Map.Entry::getKey, e -> fromNMS(e.getValue())));

        Advancement.Builder builder = Advancement.builder()
                .key(fromNMS(a.getName()))
                .display(display)
                .reward(fromNMS(a.d()))
                .criteria(criteria);

        if (a.b() != null) builder.parent(fromNMS(a.b()));
        if (a.i().length == 1) builder.flags(AFlag.MERGE_CRITERIA);
        if (a.c().i()) builder.flags(AFlag.MESSAGE);
        if (getBoolean(a.c(), "g")) builder.flags(AFlag.TOAST);
        if (a.c().j()) builder.flags(AFlag.HIDDEN);

        return builder.build();
    }

    // Implementation

    @Override
    public void update(Player p) {
        EntityPlayer sp = toNMS(p);

        sp.getAdvancementData().b(sp);
    }

    @Override
    public void register(Advancement a) {
        MinecraftKey key = toNMS(a.getKey());
        net.minecraft.server.v1_12_R1.Advancement nms = toNMS(a);

        if (AdvancementDataWorld.REGISTRY.advancements.containsKey(nms.getName())) throw new IllegalStateException("Advancement is already registered");
        AdvancementDataWorld.REGISTRY.a(Map.of(key, nms.a()));
    }

    @Override
    public Advancement getAdvancement(NamespacedKey key) {
        net.minecraft.server.v1_12_R1.Advancement nms = manager.a(toNMS(key));
        if (nms == null) return null;

        return fromNMS(nms);
    }

    @Override
    public boolean isRegistered(NamespacedKey key) {
        return AdvancementDataWorld.REGISTRY.advancements.containsKey(toNMS(key));
    }

    @Override
    public void unregister(NamespacedKey key) {
        AdvancementDataWorld.REGISTRY.advancements.remove(toNMS(key));
        Bukkit.getOnlinePlayers().forEach(p -> removeAdvancement(p, Set.of(key)));
    }

    @Override
    public void addAdvancement(Player p, Set<Advancement> advancements) {
        EntityPlayer sp = toNMS(p);

        Set<net.minecraft.server.v1_12_R1.Advancement> added = new HashSet<>();
        Map<MinecraftKey, AdvancementProgress> map = new HashMap<>();

        for (Advancement a : advancements) {
            net.minecraft.server.v1_12_R1.Advancement nms = toNMS(a);
            if (!isRegistered(a.getKey())) register(a);

            try {
                Method regListeners = AdvancementDataPlayer.class.getDeclaredMethod("c", net.minecraft.server.v1_12_R1.Advancement.class);
                regListeners.setAccessible(true);
                regListeners.invoke(sp.getAdvancementData(), nms);
            } catch (ReflectiveOperationException e) {
                throw new RuntimeException(e);
            }

            AdvancementProgress prog = sp.getAdvancementData().getProgress(nms);
            added.add(nms);
            map.put(nms.getName(), prog);
        }

        sp.playerConnection.sendPacket(new PacketPlayOutAdvancements(false, added, Set.of(), map));
        sp.getAdvancementData().b(sp);
        sp.getAdvancementData().b();
    }

    @Override
    public void removeAdvancement(Player p, Set<NamespacedKey> key) {
        EntityPlayer sp = toNMS(p);
        Set<MinecraftKey> removed = key.stream().map(Wrapper1_12_R1::toNMS).collect(Collectors.toSet());
        sp.playerConnection.sendPacket(new PacketPlayOutAdvancements(false, Set.of(), removed, Map.of()));
    }

    @Override
    public AProgress getProgress(Player p, NamespacedKey key) {
        if (!isRegistered(key)) throw new IllegalArgumentException("Advancement is not registered");
        EntityPlayer sp = toNMS(p);
        net.minecraft.server.v1_12_R1.Advancement nms = manager.a(toNMS(key));

        return new AProgress1_12_R1(p, nms, sp.getAdvancementData().getProgress(nms));
    }

    @Override
    public org.bukkit.advancement.Advancement toBukkit(Advancement a) {
        if (a == null) throw new IllegalArgumentException("Advancement cannot be null");
        return toNMS(a).bukkit;
    }

    @Override
    public Advancement fromBukkit(org.bukkit.advancement.Advancement a) {
        if (a == null) throw new IllegalArgumentException("Advancement cannot be null");

        return fromNMS(((CraftAdvancement) a).getHandle());
    }

    @Override
    public Advancement getSelectedTab(Player p) {
        EntityPlayer sp = toNMS(p);

        net.minecraft.server.v1_12_R1.Advancement lastSelectedTab = getObject(sp.getAdvancementData(), "l", net.minecraft.server.v1_12_R1.Advancement.class);
        if (lastSelectedTab == null) return null;

        return fromNMS(lastSelectedTab);
    }

    @Override
    public void setSelectedTab(Player p, Advancement advancement) {
        Advancement a0 = advancement.getRoot();
        
        if (!isRegistered(a0.getKey())) register(a0);
        EntityPlayer sp = toNMS(p);

        sp.getAdvancementData().a(toNMS(a0));
        sp.getAdvancementData().b(sp);
        sp.getAdvancementData().b();
    }
}
