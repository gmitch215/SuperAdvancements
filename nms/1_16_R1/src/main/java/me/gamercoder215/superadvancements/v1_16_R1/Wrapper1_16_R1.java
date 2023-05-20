package me.gamercoder215.superadvancements.v1_16_R1;

import com.google.common.collect.ImmutableMap;
import com.mojang.serialization.Decoder;
import com.mojang.serialization.Encoder;
import com.mojang.serialization.MapCodec;
import me.gamercoder215.superadvancements.advancement.Advancement;
import me.gamercoder215.superadvancements.advancement.*;
import me.gamercoder215.superadvancements.advancement.criteria.ACriteria;
import me.gamercoder215.superadvancements.advancement.criteria.trigger.ATrigger;
import me.gamercoder215.superadvancements.advancement.criteria.trigger.ATriggerPredicate;
import me.gamercoder215.superadvancements.util.Range;
import me.gamercoder215.superadvancements.wrapper.Wrapper;
import net.minecraft.server.v1_16_R1.*;
import net.minecraft.server.v1_16_R1.CriterionConditionEntity.b;
import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.*;
import org.bukkit.block.Biome;
import org.bukkit.block.BlockState;
import org.bukkit.craftbukkit.v1_16_R1.CraftServer;
import org.bukkit.craftbukkit.v1_16_R1.CraftWorld;
import org.bukkit.craftbukkit.v1_16_R1.advancement.CraftAdvancement;
import org.bukkit.craftbukkit.v1_16_R1.block.CraftBlock;
import org.bukkit.craftbukkit.v1_16_R1.block.CraftBlockState;
import org.bukkit.craftbukkit.v1_16_R1.enchantments.CraftEnchantment;
import org.bukkit.craftbukkit.v1_16_R1.entity.CraftPlayer;
import org.bukkit.craftbukkit.v1_16_R1.inventory.CraftItemStack;
import org.bukkit.craftbukkit.v1_16_R1.potion.CraftPotionUtil;
import org.bukkit.craftbukkit.v1_16_R1.util.CraftMagicNumbers;
import org.bukkit.craftbukkit.v1_16_R1.util.CraftNamespacedKey;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Player;
import org.bukkit.inventory.EquipmentSlot;
import org.bukkit.inventory.ItemStack;
import org.bukkit.potion.PotionType;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.*;
import java.util.function.Supplier;
import java.util.stream.Collectors;

@SuppressWarnings({"unchecked", "rawtypes"})
final class Wrapper1_16_R1 implements Wrapper {

    static {
        CraftServer server = (CraftServer) Bukkit.getServer();
        if (server != null)
            manager = server.getServer().getAdvancementData();
        else
            manager = new AdvancementDataWorld(new LootPredicateManager());
    }

    private static final AdvancementDataWorld manager;

    public static CriterionConditionValue.IntegerRange toIntRange(Range r) {
        if (r == null) return CriterionConditionValue.IntegerRange.e;

        try {
            Constructor<CriterionConditionValue.IntegerRange> intRangeC = CriterionConditionValue.IntegerRange.class.getDeclaredConstructor(Integer.class, Integer.class);
            intRangeC.setAccessible(true);
            return intRangeC.newInstance((int) r.getMinimum(), (int) r.getMaximum());
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }
    }

    public static CriterionConditionValue.FloatRange toFloatRange(Range r) {
        if (r == null) return CriterionConditionValue.FloatRange.e;

        try {
            Constructor<CriterionConditionValue.FloatRange> floatRangeC = CriterionConditionValue.FloatRange.class.getDeclaredConstructor(Float.class, Float.class);
            floatRangeC.setAccessible(true);
            return floatRangeC.newInstance((float) r.getMinimum(), (float) r.getMaximum());
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }
    }

    public static Range fromNMS(CriterionConditionValue.FloatRange r) {
        if (r == null) return Range.ANY;
        return new Range(r.a(), r.b());
    }

    public static Range fromNMS(CriterionConditionValue.IntegerRange r) {
        if (r == null) return Range.ANY;
        return new Range(r.a(), r.b());
    }

    public static EntityPlayer toNMS(Player p) {
        return ((CraftPlayer) p).getHandle();
    }

    public static MinecraftKey toNMS(NamespacedKey key) {
        if (key == null) return null;
        return CraftNamespacedKey.toMinecraft(key);
    }

    public static net.minecraft.server.v1_16_R1.ItemStack toNMS(ItemStack item) {
        if (item == null) return null;
        return CraftItemStack.asNMSCopy(item);
    }

    public static Block toNMS(Material m) {
        if (m == null) return null;
        return Block.asBlock(CraftItemStack.asNMSCopy(new ItemStack(m)).getItem());
    }

    public static net.minecraft.server.v1_16_R1.Enchantment toNMS(Enchantment enchantment) {
        return CraftEnchantment.getRaw(enchantment);
    }

    public static PotionRegistry toNMS(PotionType type) {
        if (type == null) return null;
        MinecraftKey loc = new MinecraftKey(type.name().toLowerCase());
        return IRegistry.POTION.get(loc);
    }

    public static EntityTypes<?> toNMS(EntityType type) {
        if (type == null) return null;
        MinecraftKey loc = CraftNamespacedKey.toMinecraft(type.getKey());
        return IRegistry.ENTITY_TYPE.get(loc);
    }

    public static Set<IBlockState.a> toNMS(BlockState state) {
        if (state == null) return Set.of();

        IBlockData nms = ((CraftBlockState) state).getHandle();
        Set<IBlockState.a> set = new HashSet<>();

        for (IBlockState p : nms.getBlock().getStates().d())
            set.add(p.b(nms.get(p)));

        return set;
    }

    public static CriterionTriggerProperties toNMSP(BlockState state) {
        if (state == null) return CriterionTriggerProperties.a;
        Set<IBlockState.a> set = toNMS(state);

        CriterionTriggerProperties.a builder;
        try {
            Class<?> clazz = Class.forName("net.minecraft.server.v1_16_R1.CriterionTriggerProperties$a");
            Method createBuilder = clazz.getDeclaredMethod("a");
            builder = (CriterionTriggerProperties.a) createBuilder.invoke(null);
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }

        // Gradle will not compile without casting
        set.forEach(v -> builder.a(v.a(), (Comparable & INamable) v.b()));

        return builder.b();
    }

    public static <T> IRegistryWritable<T> getRegistry(ResourceKey<IRegistry<T>> key) {
        DedicatedServer server = ((CraftServer) Bukkit.getServer()).getServer();
        return server.f.a(key).orElseThrow();
    }

    public static <T> ResourceKey<T> getKey(Keyed keyed, ResourceKey<IRegistry<T>> registry) {
        return getKey(keyed.getKey(), registry);
    }

    public static <T> ResourceKey<T> getKey(NamespacedKey key, ResourceKey<IRegistry<T>> registry) {
        if (key == null) return null;
        MinecraftKey loc = CraftNamespacedKey.toMinecraft(key);
        return ResourceKey.a(registry, loc);
    }

    public static CriterionConditionLight toNMS(ATriggerPredicate.Light predicate) {
        if (predicate == null) return CriterionConditionLight.a;

        try {
            Constructor<CriterionConditionLight> lightC = CriterionConditionLight.class.getDeclaredConstructor(CriterionConditionValue.IntegerRange.class);
            lightC.setAccessible(true);
            return lightC.newInstance(toIntRange(predicate.getBrightnessRange()));
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }
    }

    public static CriterionConditionBlock.a toNMS(ATriggerPredicate.Block predicate) {
        try {
            Method createBuilder = CriterionConditionBlock.a.class.getDeclaredMethod("a");
            CriterionConditionBlock.a builder = (CriterionConditionBlock.a) createBuilder.invoke(null);

            if (predicate == null) return builder;
            if (!predicate.getMaterials().isEmpty()) builder.a(toNMS(predicate.getMaterials().stream().findFirst().orElseThrow()));

            return builder;
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }
    }

    public static CriterionConditionLocation.a toNMS(ATriggerPredicate.Location predicate) {
        try {
            Method createBuilder = CriterionConditionLocation.a.class.getDeclaredMethod("a");
            CriterionConditionLocation.a builder = (CriterionConditionLocation.a) createBuilder.invoke(null);

            if (predicate == null) return builder;
            builder.a(predicate.isSmokey());

            setObject(builder, "a", toFloatRange(predicate.getXRange()));
            setObject(builder, "b", toFloatRange(predicate.getYRange()));
            setObject(builder, "c", toFloatRange(predicate.getZRange()));

            if (predicate.getBlockPredicate() != null) builder.a(toNMS(predicate.getBlockPredicate()).b());

            if (predicate.getBiome() != null) setObject(builder, "d", IRegistry.BIOME.get(toNMS(predicate.getBiome().getKey())));
            if (predicate.getLightPredicate() != null) setObject(builder, "h", toNMS(predicate.getLightPredicate()));

            if (predicate.getDimension() != null) {
                WorldServer nms = ((CraftWorld) predicate.getDimension()).getHandle();
                NamespacedKey key = CraftNamespacedKey.fromMinecraft(nms.getDimensionKey().a());
                setObject(builder, "f", getKey(key, IRegistry.L));
            }

            return builder;
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }
    }

    public static CriterionConditionEnchantments toNMS(ATriggerPredicate.Enchantment predicate) {
        if (predicate == null) return CriterionConditionEnchantments.a;
        return new CriterionConditionEnchantments(
                toNMS(predicate.getEnchantment()),
                toIntRange(predicate.getLevelRange())
        );
    }

    public static CriterionConditionItem.a toNMS(ATriggerPredicate.Item predicate) {
        try {
            Method createBuilder = CriterionConditionItem.a.class.getDeclaredMethod("a");
            CriterionConditionItem.a builder = (CriterionConditionItem.a) createBuilder.invoke(null);

            if (predicate == null) return builder;

            if (!predicate.getIncludes().isEmpty()) builder.a(predicate.getIncludes().stream()
                            .map(Wrapper1_16_R1::toNMS)
                            .findFirst()
                            .orElseThrow().getItem());

            setObject(builder, "e", toIntRange(predicate.getCountRange()));
            setObject(builder, "f", toIntRange(predicate.getDurabilityRange()));

            predicate.getEnchantments().stream().map(Wrapper1_16_R1::toNMS).forEach(builder::a);

            setObject(builder, "b", predicate.getStoredEnchantments()
                    .stream()
                    .map(Wrapper1_16_R1::toNMS)
                    .collect(Collectors.toList())
            );

            return builder;
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }
    }

    public static CriterionConditionEntity.a toNMS(ATriggerPredicate.Entity predicate) {
        try {
            Method createBuilder = CriterionConditionEntity.a.class.getDeclaredMethod("a");
            CriterionConditionEntity.a builder = (CriterionConditionEntity.a) createBuilder.invoke(null);

            if (predicate == null) return builder;

            if (predicate.getType() != null) builder.a(CriterionConditionEntityType.b(toNMS(predicate.getType())));
            if (predicate.getPlayerDistanceRange() != null)
                builder.a(CriterionConditionDistance.a(toFloatRange(predicate.getPlayerDistanceRange())));
            if (!predicate.getEquipment().isEmpty()) {
                Method createEquipmentBuilder = CriterionConditionEntityEquipment.a.class.getDeclaredMethod("a");
                CriterionConditionEntityEquipment.a equipment = (CriterionConditionEntityEquipment.a) createEquipmentBuilder.invoke(null);
                for (Map.Entry<EquipmentSlot, ATriggerPredicate.Item> entry : predicate.getEquipment().entrySet()) {
                    switch (entry.getKey()) {
                        case HEAD: equipment.a(toNMS(entry.getValue()).b()); break;
                        case CHEST: equipment.b(toNMS(entry.getValue()).b()); break;
                        case LEGS: equipment.c(toNMS(entry.getValue()).b()); break;
                        case FEET: equipment.d(toNMS(entry.getValue()).b()); break;
                        case HAND: setObject(equipment, "e", toNMS(entry.getValue()).b()); break;
                        case OFF_HAND: setObject(equipment, "f", toNMS(entry.getValue()).b()); break;
                    }
                }

                builder.a(equipment.b());
            }

            Method createFlagsBuilder = CriterionConditionEntityFlags.a.class.getDeclaredMethod("a");
            CriterionConditionEntityFlags.a flags = (CriterionConditionEntityFlags.a) createFlagsBuilder.invoke(null);

            setObject(flags, "a", predicate.isOnFire());
            setObject(flags, "b", predicate.isCrouching());
            setObject(flags, "c", predicate.isSprinting());
            setObject(flags, "d", predicate.isSwimming());
            setObject(flags, "e", predicate.isBaby());

            setObject(builder, "f", flags);

            if (predicate.getTarget() != null) builder.b(toNMS(predicate.getTarget()).b());
            if (predicate.getVehicle() != null) builder.a(toNMS(predicate.getVehicle()).b());

            return builder;
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }
    }

    public static CriterionConditionDamage toNMS(ATriggerPredicate.Damage predicate) {
        try {
            Method createBuilder = CriterionConditionDamage.a.class.getDeclaredMethod("a");
            CriterionConditionDamage.a builder = (CriterionConditionDamage.a) createBuilder.invoke(null);

            if (predicate == null) return CriterionConditionDamage.a;

            setObject(builder, "a", toFloatRange(predicate.getDealtRange()));
            setObject(builder, "b", toFloatRange(predicate.getTakenRange()));
            builder.a(predicate.wasBlocked());

            if (predicate.getSourcePredicate() != null) setObject(builder, "c", toNMSP(predicate.getSourcePredicate()));

            return builder.b();
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }
    }

    public static CriterionConditionEntity toNMSP(ATriggerPredicate.Entity predicate) {
        if (predicate == null) return CriterionConditionEntity.a;
        return toNMS(predicate).b();
    }

    public static CriterionConditionItem toNMSP(ATriggerPredicate.Item predicate) {
        if (predicate == null) return CriterionConditionItem.a;
        return toNMS(predicate).b();
    }

    public static CriterionConditionLocation toNMSP(ATriggerPredicate.Location predicate) {
        if (predicate == null) return CriterionConditionLocation.a;
        return toNMS(predicate).b();
    }

    public static CriterionConditionDistance toNMSP(Range range) {
        if (range == null) return CriterionConditionDistance.a;
        return CriterionConditionDistance.a(toFloatRange(range));
    }

    public static CriterionInstance toNMS(ATrigger trigger) {
        if (trigger == null) return null;

        Map<String, Object> c = trigger.getConditions();
        switch (trigger.getKey().getKey()) {
            case "impossible": new CriterionTriggerImpossible.a();
            case "bee_nest_destroyed": {
                Material block = (Material) c.get("block");
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                Range beesInside = (Range) c.get("num_bees_inside");

                return CriterionTriggerBeeNestDestroyed.a.a(toNMS(block), toNMS(item), toIntRange(beesInside));
            }
            case "bred_animals": {
                ATriggerPredicate.Entity child = (ATriggerPredicate.Entity) c.get("child");
                ATriggerPredicate.Entity parent1 = (ATriggerPredicate.Entity) c.get("parent");
                ATriggerPredicate.Entity parent2 = (ATriggerPredicate.Entity) c.get("partner");

                return CriterionTriggerBredAnimals.a.a(toNMSP(parent1), toNMSP(parent2), toNMSP(child));
            }
            case "brewed_potion": {
                PotionType potion = (PotionType) c.get("potion");
                return new CriterionTriggerBrewedPotion.a(b.a, toNMS(potion));
            }
            case "changed_dimension": {
                NamespacedKey from = null;
                NamespacedKey to = null;

                if (c.get("from") != null) from = fromNMS(((CraftWorld) c.get("from")).getHandle().getDimensionKey().a());
                if (c.get("to") != null) to = fromNMS(((CraftWorld) c.get("to")).getHandle().getDimensionKey().a());
                return new CriterionTriggerChangedDimension.a(b.a, getKey(from, IRegistry.ae), getKey(to, IRegistry.ae));
            }
            case "channeled_lightning": {
                ATriggerPredicate.Entity[] victims = (ATriggerPredicate.Entity[]) c.get("victims");
                if (victims == null) return CriterionTriggerChanneledLightning.a.a(new CriterionConditionEntity[0]);

                return CriterionTriggerChanneledLightning.a.a(
                        Arrays.stream(victims)
                                .map(Wrapper1_16_R1::toNMSP)
                                .toArray(CriterionConditionEntity[]::new)
                );
            }
            case "construct_beacon": {
                Range level = (Range) c.get("level");
                return CriterionTriggerConstructBeacon.a.a(toIntRange(level));
            }
            case "consume_item": {
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                return new CriterionTriggerConsumeItem.a(b.a, toNMSP(item));
            }
            case "cured_zombie_villager": {
                ATriggerPredicate.Entity villager = (ATriggerPredicate.Entity) c.get("villager");
                ATriggerPredicate.Entity zombie = (ATriggerPredicate.Entity) c.get("zombie");
                return new CriterionTriggerCuredZombieVillager.a(b.a, b.a(toNMSP(zombie)), b.a(toNMSP(villager)));
            }
            case "enchanted_item": {
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                Range levels = (Range) c.get("levels");
                return new CriterionTriggerEnchantedItem.a(b.a, toNMSP(item), toIntRange(levels));
            }
            case "enter_block": {
                Material block = (Material) c.get("block");
                BlockState state = (BlockState) c.get("state");
                return new CriterionTriggerEnterBlock.a(b.a, toNMS(block), toNMSP(state));
            }
            case "entity_hurt_player": {
                ATriggerPredicate.Damage damage = (ATriggerPredicate.Damage) c.get("damage");
                return new CriterionTriggerEntityHurtPlayer.a(b.a, toNMS(damage));
            }
            case "entity_killed_player": 
            case "player_killed_entity": {
                ATriggerPredicate.Entity entity = (ATriggerPredicate.Entity) c.get("entity");
                return new CriterionTriggerKilled.a(new MinecraftKey(trigger.getKey().getKey()), b.a, b.a(toNMSP(entity)), CriterionConditionDamageSource.a);
            }
            case "fishing_rod_hooked": {
                ATriggerPredicate.Item rod = (ATriggerPredicate.Item) c.get("rod");
                ATriggerPredicate.Entity entity = (ATriggerPredicate.Entity) c.get("entity");
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                return CriterionTriggerFishingRodHooked.a.a(toNMSP(rod), toNMSP(entity), toNMSP(item));
            }
            case "hero_of_the_village": CriterionTriggerLocation.a.d();
            case "inventory_changed": {
                Range emptySlots = (Range) c.get("empty_slots");
                Range fullSlots = (Range) c.get("full_slots");
                Range occupiedSlots = (Range) c.get("occupied_slots");
                Set<ATriggerPredicate.Item> items = (Set<ATriggerPredicate.Item>) c.get("items");

                CriterionConditionItem[] predicates = new CriterionConditionItem[0];
                if (items != null)
                    predicates = items.stream()
                            .map(Wrapper1_16_R1::toNMSP)
                            .toArray(CriterionConditionItem[]::new);

                return new CriterionTriggerInventoryChanged.a(b.a, toIntRange(emptySlots), toIntRange(fullSlots), toIntRange(occupiedSlots), predicates);
            }
            case "item_durability_changed": {
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                Range delta = (Range) c.get("delta");
                Range durability = (Range) c.get("durability");
                return new CriterionTriggerItemDurabilityChanged.a(b.a, toNMSP(item), toIntRange(delta), toIntRange(durability));
            }
            case "item_used_on_block": {
                ATriggerPredicate.Location location = (ATriggerPredicate.Location) c.get("location");
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                return CriterionTriggerInteractBlock.a.a(toNMS(location), toNMS(item));
            }
            case "killed_by_crossbow": {
                Range uniqueTypes = (Range) c.get("unique_entity_types");
                Set<ATriggerPredicate.Entity> victims = (Set<ATriggerPredicate.Entity>) c.get("victims");

                b[] predicates = new b[0];
                if (victims != null)
                    predicates = victims.stream()
                            .map(Wrapper1_16_R1::toNMSP)
                            .map(b::a)
                            .toArray(b[]::new);

                return new CriterionTriggerKilledByCrossbow.a(b.a, predicates, toIntRange(uniqueTypes));
            }
            case "levitation": {
                Range distance = (Range) c.get("distance");
                Range duration = (Range) c.get("duration");
                return new CriterionTriggerLevitation.a(b.a, toNMSP(distance), toIntRange(duration));
            }
            case "location": new CriterionTriggerLocation.a(new MinecraftKey("location"), b.a, CriterionConditionLocation.a);
            case "nether_travel": {
                ATriggerPredicate.Location startPosition = (ATriggerPredicate.Location) c.get("start_position");
                Range distance = (Range) c.get("distance");
                return new CriterionTriggerNetherTravel.a(b.a, toNMSP(startPosition), CriterionConditionLocation.a, toNMSP(distance));
            }
            case "placed_block": {
                Material block = (Material) c.get("block");
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                ATriggerPredicate.Location location = (ATriggerPredicate.Location) c.get("location");
                BlockState state = (BlockState) c.get("state");

                return new CriterionTriggerPlacedBlock.a(b.a, toNMS(block), toNMSP(state), toNMSP(location), toNMSP(item));
            }
            case "player_generates_container_loot": {
                NamespacedKey lootTable = (NamespacedKey) c.get("loot_table");
                return CriterionTriggerPlayerGeneratesContainerLoot.a.a(toNMS(lootTable));
            }
            case "player_hurt_entity": {
                ATriggerPredicate.Damage damage = (ATriggerPredicate.Damage) c.get("damage");
                ATriggerPredicate.Entity entity = (ATriggerPredicate.Entity) c.get("entity");
                return new CriterionTriggerPlayerHurtEntity.a(b.a, toNMS(damage), b.a(toNMSP(entity)));
            }
            case "player_interacted_with_entity": {
                ATriggerPredicate.Entity entity = (ATriggerPredicate.Entity) c.get("entity");
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                return CriterionTriggerPlayerInteractedWithEntity.a.a(b.a, toNMS(item), b.a(toNMSP(entity)));
            }
            case "recipe_unlocked": {
                NamespacedKey recipe = (NamespacedKey) c.get("recipe");
                return new CriterionTriggerRecipeUnlocked.a(b.a, toNMS(recipe));
            }
            case "shot_crossbow": {
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                return new CriterionTriggerShotCrossbow.a(b.a, toNMSP(item));
            }
            case "slept_in_bed": CriterionTriggerLocation.a.c();
            case "slide_down_block": {
                Material block = (Material) c.get("block");
                BlockState state = (BlockState) c.get("state");
                return new CriterionSlideDownBlock.a(b.a, toNMS(block), toNMSP(state));
            }
            case "tame_animal": {
                ATriggerPredicate.Entity entity = (ATriggerPredicate.Entity) c.get("entity");
                return CriterionTriggerTamedAnimal.a.a(toNMSP(entity));
            }
            case "target_hit": {
                Range signalStrength = (Range) c.get("signal_strength");
                ATriggerPredicate.Entity projectile = (ATriggerPredicate.Entity) c.get("projectile");
                return CriterionTriggerTargetHit.a.a(toIntRange(signalStrength), b.a(toNMSP(projectile)));
            }
            case "tick": new CriterionTriggerTick.a(b.a);
            case "used_ender_eye": {
                Range distance = (Range) c.get("distance");
                return new CriterionTriggerUsedEnderEye.a(b.a, toFloatRange(distance));
            }
            case "used_totem": {
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                return new CriterionTriggerUsedTotem.a(b.a, toNMSP(item));
            }
            case "villager_trade": {
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                ATriggerPredicate.Entity villager = (ATriggerPredicate.Entity) c.get("villager");
                return new CriterionTriggerVillagerTrade.a(b.a, b.a(toNMSP(villager)), toNMSP(item));
            }
            case "voluntary_exile": return new CriterionTriggerLocation.a(new MinecraftKey("voluntary_exile"), b.a, CriterionConditionLocation.a);
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
                fromNMS(getObject(t, "c", CriterionConditionValue.IntegerRange.class))
            );
            case "bred_animals": return ATrigger.bredAnimals(
                fromNMS(getObject(t, "c", b.class)),
                fromNMS(getObject(t, "a", b.class)),
                fromNMS(getObject(t, "b", b.class))
            );
            case "brewed_potion": return ATrigger.brewedPotion(
                fromNMS(getObject(t, "a", PotionRegistry.class))
            );
            case "changed_dimension": return ATrigger.changedDimension(
                fromNMSW(getObject(t, "a", ResourceKey.class)),
                fromNMSW(getObject(t, "b", ResourceKey.class))
            );
            case "channeled_lightning": return ATrigger.channeledLightning(
                Arrays.stream(getObject(t, "a", b[].class))
                        .map(Wrapper1_16_R1::fromNMS)
                        .collect(Collectors.toSet())
            );
            case "construct_beacon": return ATrigger.constructBeacon(
                fromNMS(getObject(t, "a", CriterionConditionValue.IntegerRange.class))
            );
            case "consume_item": return ATrigger.consumeItem(
                fromNMS(getObject(t, "a", CriterionConditionItem.class))
            );
            case "cured_zombie_villager": return ATrigger.curedZombieVillager(
                fromNMS(getObject(t, "a", b.class)),
                fromNMS(getObject(t, "b", b.class))
            );
            case "enchanted_item": return ATrigger.enchantedItem(
                fromNMS(getObject(t, "a", CriterionConditionItem.class)),
                fromNMS(getObject(t, "b", CriterionConditionValue.IntegerRange.class))
            );
            case "enter_block": return ATrigger.enterBlock(
                fromNMS(getObject(t, "a", Block.class)),
                fromNMS(getObject(t, "a", Block.class), getObject(t, "b", CriterionTriggerProperties.class))
            );
            case "entity_hurt_player": return ATrigger.entityHurtPlayer(
                fromNMS(getObject(t, "a", CriterionConditionDamage.class))
            );
            case "entity_killed_player": return ATrigger.entityKilledPlayer(
                fromNMS(getObject(t, "a", b.class))
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
                fromNMS(getObject(t, "c", CriterionConditionValue.IntegerRange.class)),
                fromNMS(getObject(t, "b", CriterionConditionValue.IntegerRange.class)),
                fromNMS(getObject(t, "a", CriterionConditionValue.IntegerRange.class)),
                Arrays.stream(getObject(t, "d", CriterionConditionItem[].class))
                        .map(Wrapper1_16_R1::fromNMS)
                        .collect(Collectors.toSet())
            );
            case "item_durability_changed": return ATrigger.itemDurabilityChanged(
                fromNMS(getObject(t, "a", CriterionConditionItem.class)),
                fromNMS(getObject(t, "c", CriterionConditionValue.IntegerRange.class)),
                fromNMS(getObject(t, "b", CriterionConditionValue.IntegerRange.class))
            );
            case "item_used_on_block": return ATrigger.itemUsedOnBlock(
                fromNMS(getObject(t, "a", CriterionConditionLocation.class)),
                fromNMS(getObject(t, "b", CriterionConditionItem.class))
            );
            case "kill_mob_near_sculk_catalyst": return ATrigger.killMobNearSculkCatalyst(
                fromNMS(getObject(t, "a", b.class))
            );
            case "killed_by_crossbow": return ATrigger.killedByCrossbow(
                fromNMS(getObject(t, "b", CriterionConditionValue.IntegerRange.class)),
                Arrays.stream(getObject(t, "a", b[].class))
                        .map(Wrapper1_16_R1::fromNMS)
                        .collect(Collectors.toSet())
            );
            case "levitation": return ATrigger.levitation(
                fromNMS(getObject(t, "a", CriterionConditionDistance.class)),
                fromNMS(getObject(t, "b", CriterionConditionValue.IntegerRange.class))
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
                fromNMS(getObject(t, "a", Block.class), getObject(t, "b", CriterionTriggerProperties.class))
            );
            case "player_generates_container_loot": return ATrigger.playerGeneratesContainerLoot(
                fromNMS(getObject(t, "a", MinecraftKey.class))
            );
            case "player_hurt_entity": return ATrigger.playerHurtEntity(
                fromNMS(getObject(t, "a", CriterionConditionDamage.class)),
                fromNMS(getObject(t, "b", b.class))
            );
            case "player_interacted_with_entity": return ATrigger.playerInteractedWithEntity(
                fromNMS(getObject(t, "b", b.class)),
                fromNMS(getObject(t, "a", CriterionConditionItem.class))
            );
            case "player_killed_entity": return ATrigger.playerKilledEntity(
                fromNMS(getObject(t, "a", b.class))
            );
            case "recipe_unlocked": return ATrigger.recipeUnlocked(
                fromNMS(getObject(t, "a", MinecraftKey.class))
            );
            case "ride_entity_in_lava": return ATrigger.rideEntityInLava(
                fromNMS(getObject(t, "a", CriterionConditionLocation.class)),
                fromNMS(getObject(t, "b", CriterionConditionDistance.class))
            );
            case "shot_crossbow": return ATrigger.shotCrossbow(
                fromNMS(getObject(t, "a", CriterionConditionItem.class))
            );
            case "slept_in_bed": return ATrigger.sleptInBed();
            case "slide_down_block": return ATrigger.slideDownBlock(
                fromNMS(getObject(t, "a", Block.class)),
                fromNMS(getObject(t, "a", Block.class), getObject(t, "b", CriterionTriggerProperties.class))
            );
            case "started_riding": return ATrigger.startedRiding();
            case "tame_animal": return ATrigger.tameAnimal(
                fromNMS(getObject(t, "a", b.class))
            );
            case "target_hit": return ATrigger.targetHit(
                fromNMS(getObject(t, "a", CriterionConditionValue.IntegerRange.class)),
                fromNMS(getObject(t, "b", b.class))
            );
            case "thrown_item_picked_up_by_entity": return ATrigger.thrownItemPickedUpByEntity(
                fromNMS(getObject(t, "a", CriterionConditionItem.class)),
                fromNMS(getObject(t, "b", b.class))
            );
            case "thrown_item_picked_up_by_player": return ATrigger.thrownItemPickedUpByPlayer(
                fromNMS(getObject(t, "a", CriterionConditionItem.class)),
                fromNMS(getObject(t, "b", b.class))
            );
            case "tick": return ATrigger.tick();
            case "used_ender_eye": return ATrigger.usedEnderEye(
                fromNMS(getObject(t, "a", CriterionConditionValue.FloatRange.class))
            );
            case "used_totem": return ATrigger.usedTotem(
                fromNMS(getObject(t, "a", CriterionConditionItem.class))
            );
            case "using_item": return ATrigger.usingItem(
                fromNMS(getObject(t, "a", CriterionConditionItem.class))
            );
            case "villager_trade": return ATrigger.villagerTrade(
                fromNMS(getObject(t, "b", CriterionConditionItem.class)),
                fromNMS(getObject(t, "a", b.class))
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
                        .map(Wrapper1_16_R1::toNMS)
                        .toArray(MinecraftKey[]::new),
                reward.getRecipes() == null ? null : reward.getRecipes().stream()
                        .map(Keyed::getKey)
                        .map(Wrapper1_16_R1::toNMS)
                        .toArray(MinecraftKey[]::new),
                CustomFunction.a.a
        );
    }

    public static net.minecraft.server.v1_16_R1.Advancement toNMS(Advancement a) {
        if (a == null) return null;
        if (manager.REGISTRY.a(toNMS(a.getKey())) != null) return manager.REGISTRY.a(toNMS(a.getKey()));

        ADisplay display = a.getDisplay();
        String title = display.getTitleAsString();
        String desc = display.getDescriptionAsString();
        AdvancementFrameType frame = Arrays.stream(AdvancementFrameType.values()).filter(f -> f.a().equalsIgnoreCase(display.getFrame().name())).findFirst().orElse(AdvancementFrameType.TASK);
        MinecraftKey bg = null;

        if (a.getParent() == null && display.getBackgroundTexture() != null)
            bg = new MinecraftKey(display.getBackgroundTexture());

        AdvancementDisplay nmsDisplay = new AdvancementDisplay(toNMS(display.getIcon()), new ChatComponentText(title), new ChatComponentText(desc), bg, frame, a.hasFlag(AFlag.TOAST), a.hasFlag(AFlag.MESSAGE), a.hasFlag(AFlag.HIDDEN));
        nmsDisplay.a(display.getX(), display.getY());

        net.minecraft.server.v1_16_R1.Advancement parent = a.getParent() == null ? null : toNMS(a.getParent());

        return new net.minecraft.server.v1_16_R1.Advancement(
                toNMS(a.getKey()),
                parent,
                nmsDisplay,
                toNMS(a.getReward()),
                a.getCriteria().entrySet().stream()
                        .collect(Collectors.toMap(Map.Entry::getKey, e -> toNMS(e.getValue()))),
                a.hasFlag(AFlag.MERGE_CRITERIA) ? AdvancementRequirements.OR.createRequirements(a.getCriteria().keySet()) : AdvancementRequirements.AND.createRequirements(a.getCriteria().keySet())
        );
    }

    public static ItemStack fromNMS(net.minecraft.server.v1_16_R1.ItemStack item) {
        if (item == null) return null;
        return CraftItemStack.asBukkitCopy(item);
    }

    public static NamespacedKey fromNMS(MinecraftKey key) {
        if (key == null) return null;
        return CraftNamespacedKey.fromMinecraft(key);
    }

    public static Biome fromNMS(ResourceKey<BiomeBase> biome) {
        if (biome == null) return null;
        IRegistryWritable<BiomeBase> registry = getRegistry(IRegistry.u);
        return CraftBlock.biomeBaseToBiome(registry.get(biome.a()));
    }

    public static World fromNMSW(ResourceKey<net.minecraft.server.v1_16_R1.World> world) {
        if (world == null) return null;
        IRegistryWritable<net.minecraft.server.v1_16_R1.World> registry = getRegistry(IRegistry.ae);
        return registry.a(world).getWorld();
    }

    public static Material fromNMS(Block block) {
        if (block == null) return null;
        return CraftMagicNumbers.getMaterial(block);
    }

    public static EntityType fromNMS(CriterionConditionEntityType p) {
        if (p == null) return null;
        try {
            Field typeF = p.getClass().getDeclaredField("a");
            typeF.setAccessible(true);

            Class<?> typeC = typeF.getType();

            if (EntityTypes.class.isAssignableFrom(typeC)) {
                EntityTypes<?> entityType = (EntityTypes<?>) typeF.get(p);
                return EntityType.valueOf(IRegistry.ENTITY_TYPE.getKey(entityType).getKey().toUpperCase());
            } else {
                throw new IllegalArgumentException("Unknown EntityTypePredicate Field: " + typeC);
            }
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }
    }

    public static Range fromNMS(CriterionConditionDistance p) {
        if (p == null) return null;
        return fromNMS(getObject(p, "f", CriterionConditionValue.FloatRange.class));
    }

    public static ItemStack fromNMS(Item item) {
        if (item == null) return null;
        return CraftItemStack.asBukkitCopy(new net.minecraft.server.v1_16_R1.ItemStack(item));
    }

    public static Enchantment fromNMS(net.minecraft.server.v1_16_R1.Enchantment enchant) {
        if (enchant == null) return null;
        NamespacedKey key = fromNMS(IRegistry.ENCHANTMENT.getKey(enchant));
        return CraftEnchantment.getByKey(key);
    }

    public static PotionType fromNMS(PotionRegistry potion) {
        if (potion == null) return null;
        MinecraftKey loc = IRegistry.POTION.getKey(potion);
        return CraftPotionUtil.toBukkit(loc.getKey()).getType();
    }

    public static BlockState fromNMS(Block block, CriterionTriggerProperties predicate) {
        if (predicate == null) return null;
        List<Object> properties = getObject(predicate, "b", List.class);

        try {
            Class<?> propertyMatcherC = Class.forName("net.minecraft.server.v1_16_R1.CriterionTriggerProperties$c");

            Map<String, String> propertyMap = properties.stream()
            .map(o -> {
                try {
                    Field nameF = propertyMatcherC.getDeclaredField("a");
                    nameF.setAccessible(true);
                    String name = (String) nameF.get(o);

                    if (o.getClass().getDeclaredFields().length == 2) {
                        // private class RangedIBlockStateMatcher
                        Field minF = o.getClass().getDeclaredField("a");
                        String min = String.valueOf(minF.get(o));
                    
                        return new AbstractMap.SimpleEntry<>(name, min);
                    } else {
                        // private class ExactIBlockStateMatcher
                        Field valueF = o.getClass().getDeclaredField("a");
                        valueF.setAccessible(true);
                        String value = String.valueOf(valueF.get(o));

                        return new AbstractMap.SimpleEntry<>(name, value);
                    }
                } catch (ReflectiveOperationException e) {
                    throw new RuntimeException(e);
                }
            })
            .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

            Map<IBlockState<?>, Comparable<?>> prop = new HashMap<>();

            for (IBlockState<?> property : block.getStates().d()) {
                if (!propertyMap.containsKey(property.getName())) continue;

                String valueS = propertyMap.get(property.getName());
                if (valueS.equals("true") || valueS.equals("false"))
                    prop.put(property, Boolean.parseBoolean(valueS));
                else if (property.getClass().isEnum())
                    prop.put(property, Enum.valueOf(property.getClass().asSubclass(Enum.class), valueS));
                else if (property instanceof BlockStateInteger)
                    prop.put(property, Integer.parseInt(valueS));
                else
                    prop.put(property, valueS);
            }

            Supplier<IBlockData> supplier = block::getBlockData;

            IBlockData nms = new IBlockData(
                block, 
                ImmutableMap.copyOf(prop),
                MapCodec.of(Encoder.empty(), Decoder.unit(supplier))
            );

            CraftBlockState state = new CraftBlockState(fromNMS(block));
            state.setData(nms);
            return state;
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }
    }

    public static ATriggerPredicate.Entity fromNMS(b composite) {
        if (composite == null) return null;
        if (composite == b.a) return ATriggerPredicate.Entity.ANY;
        
        LootItemCondition[] conditions = getObject(composite, "b", LootItemCondition[].class);
        return fromNMS(Arrays.stream(conditions)
                .filter(c -> c instanceof LootItemConditionEntityProperty)
                .map(c -> getObject(c, "a", CriterionConditionEntity.class))
                .findFirst()
                .orElse(null));
    }

    public static ATriggerPredicate.Damage fromNMS(CriterionConditionDamage p) {
        if (p == null) return null;
        return ATriggerPredicate.Damage.builder()
                .dealt(fromNMS(getObject(p, "b", CriterionConditionValue.FloatRange.class)))
                .taken(fromNMS(getObject(p, "c", CriterionConditionValue.FloatRange.class)))
                .source(fromNMS(getObject(p, "d", CriterionConditionEntity.class)))
                .build();
    }

    public static ATriggerPredicate.Entity fromNMS(CriterionConditionEntity p) {
        if (p == null) return null;
        CriterionConditionEntityFlags flags = getObject(p, "h", CriterionConditionEntityFlags.class);
        CriterionConditionEntityEquipment eq = getObject(p, "i", CriterionConditionEntityEquipment.class);

        return ATriggerPredicate.Entity.builder()
                .type(fromNMS(getObject(p, "b", CriterionConditionEntityType.class)))
                .distanceToPlayer(fromNMS(getObject(p, "c", CriterionConditionDistance.class)))
                .location(fromNMS(getObject(p, "d", CriterionConditionLocation.class)))
                .steppingLocation(fromNMS(getObject(p, "e", CriterionConditionLocation.class)))
                // Flags
                .onFire(getBoolean(flags, "b"))
                .crouching(getBoolean(flags, "c"))
                .sprinting(getBoolean(flags, "d"))
                .swimming(getBoolean(flags, "e"))
                .baby(getBoolean(flags, "f"))
                // Equipment
                .equipment(Map.of(
                    EquipmentSlot.HEAD, fromNMS(getObject(eq, "c", CriterionConditionItem.class)),
                    EquipmentSlot.CHEST, fromNMS(getObject(eq, "d", CriterionConditionItem.class)),
                    EquipmentSlot.LEGS, fromNMS(getObject(eq, "e", CriterionConditionItem.class)),
                    EquipmentSlot.FEET, fromNMS(getObject(eq, "f", CriterionConditionItem.class)),
                    EquipmentSlot.HAND, fromNMS(getObject(eq, "g", CriterionConditionItem.class)),
                    EquipmentSlot.OFF_HAND, fromNMS(getObject(eq, "h", CriterionConditionItem.class))
                ))
                // Other
                .vehicle(fromNMS(getObject(p, "k", CriterionConditionEntity.class)))
                .passenger(fromNMS(getObject(p, "l", CriterionConditionEntity.class)))
                .target(fromNMS(getObject(p, "m", CriterionConditionEntity.class)))
                .build();
    }

    public static ATriggerPredicate.Enchantment fromNMS(CriterionConditionEnchantments p) {
        if (p == null) return null;
        return ATriggerPredicate.Enchantment.builder()
                .enchantment(fromNMS(getObject(p, "c", net.minecraft.server.v1_16_R1.Enchantment.class)))
                .level(fromNMS(getObject(p, "d", CriterionConditionValue.IntegerRange.class)))
                .build();
    }

    public static ATriggerPredicate.Item fromNMS(CriterionConditionItem p) {
        if (p == null) return null;
        ATriggerPredicate.Item.Builder builder = ATriggerPredicate.Item.builder()
                .include(((Set<Item>) getObject(p, "c", Set.class))
                        .stream()
                        .map(Wrapper1_16_R1::fromNMS)
                        .collect(Collectors.toSet())
                )
                .count(fromNMS(getObject(p, "d", CriterionConditionValue.IntegerRange.class)))
                .durability(fromNMS(getObject(p, "e", CriterionConditionValue.IntegerRange.class)));

        for (CriterionConditionEnchantments ench : getObject(p, "f", CriterionConditionEnchantments[].class))
            builder.enchantment(fromNMS(ench));
        
        for (CriterionConditionEnchantments ench : getObject(p, "g", CriterionConditionEnchantments[].class))
            builder.storedEnchantment(fromNMS(ench));

        return builder.build();
    }

    public static ATriggerPredicate.Light fromNMS(CriterionConditionLight p) {
        if (p == null) return null;
        return ATriggerPredicate.Light.of(fromNMS(getObject(p, "b", CriterionConditionValue.IntegerRange.class)));
    }

    public static ATriggerPredicate.Block fromNMS(CriterionConditionBlock p) {
        if (p == null) return null;
        return ATriggerPredicate.Block.of(((Set<Block>) getObject(p, "c", Set.class))
                .stream()
                .map(Wrapper1_16_R1::fromNMS)
                .collect(Collectors.toSet())
        );
    }

    public static ATriggerPredicate.Location fromNMS(CriterionConditionLocation p) {
        if (p == null) return null;
        return ATriggerPredicate.Location.builder()
                .x(fromNMS(getObject(p, "c", CriterionConditionValue.FloatRange.class)))
                .y(fromNMS(getObject(p, "d", CriterionConditionValue.FloatRange.class)))
                .z(fromNMS(getObject(p, "e", CriterionConditionValue.FloatRange.class)))
                .biome(fromNMS(getObject(p, "f", ResourceKey.class)))
                .dimension(fromNMSW(getObject(p, "h", ResourceKey.class)))
                .smokey(getBoolean(p, "i"))
                .light(fromNMS(getObject(p, "j", CriterionConditionLight.class)))
                .block(fromNMS(getObject(p, "k", CriterionConditionBlock.class)))
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
                    Arrays.stream(loot).map(Wrapper1_16_R1::fromNMS).collect(Collectors.toList()),
                    Arrays.stream(recipes).map(Wrapper1_16_R1::fromNMS).collect(Collectors.toList())
            );
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }
    }

    public static Advancement fromNMS(net.minecraft.server.v1_16_R1.Advancement a) {
        if (a == null) return null;
        NMSDisplay1_16_R1 display = new NMSDisplay1_16_R1(a.c());

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
        net.minecraft.server.v1_16_R1.Advancement nms = toNMS(a);

        if (manager.REGISTRY.advancements.containsKey(nms.getName())) throw new IllegalStateException("Advancement is already registered");
        manager.REGISTRY.a(Map.of(key, nms.a()));
    }

    @Override
    public Advancement getAdvancement(NamespacedKey key) {
        net.minecraft.server.v1_16_R1.Advancement nms = manager.REGISTRY.a(toNMS(key));
        if (nms == null) return null;

        return fromNMS(nms);
    }

    @Override
    public boolean isRegistered(NamespacedKey key) {
        return manager.REGISTRY.advancements.containsKey(toNMS(key));
    }

    @Override
    public void unregister(NamespacedKey key) {
        manager.REGISTRY.advancements.remove(toNMS(key));
        Bukkit.getOnlinePlayers().forEach(p -> removeAdvancement(p, Set.of(key)));
    }

    @Override
    public void addAdvancement(Player p, Set<Advancement> advancements) {
        EntityPlayer sp = toNMS(p);

        Set<net.minecraft.server.v1_16_R1.Advancement> added = new HashSet<>();
        Map<MinecraftKey, AdvancementProgress> map = new HashMap<>();

        for (Advancement a : advancements) {
            net.minecraft.server.v1_16_R1.Advancement nms = toNMS(a);
            if (!isRegistered(a.getKey())) register(a);

            try {
                Method regListeners = AdvancementDataPlayer.class.getDeclaredMethod("c", net.minecraft.server.v1_16_R1.Advancement.class);
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
        Set<MinecraftKey> removed = key.stream().map(Wrapper1_16_R1::toNMS).collect(Collectors.toSet());
        sp.playerConnection.sendPacket(new PacketPlayOutAdvancements(false, Set.of(), removed, Map.of()));
    }

    @Override
    public AProgress getProgress(Player p, NamespacedKey key) {
        if (!isRegistered(key)) throw new IllegalArgumentException("Advancement is not registered");
        EntityPlayer sp = toNMS(p);
        net.minecraft.server.v1_16_R1.Advancement nms = manager.REGISTRY.a(toNMS(key));

        return new AProgress1_16_R1(p, nms, sp.getAdvancementData().getProgress(nms));
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

        net.minecraft.server.v1_16_R1.Advancement lastSelectedTab = getObject(sp.getAdvancementData(), "l", net.minecraft.server.v1_16_R1.Advancement.class);
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
