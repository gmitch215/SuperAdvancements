package me.gamercoder215.superadvancements.v1_20_R2;

import com.google.common.collect.ImmutableMap;
import com.mojang.serialization.Decoder;
import com.mojang.serialization.Encoder;
import com.mojang.serialization.MapCodec;
import me.gamercoder215.superadvancements.advancement.Advancement;
import me.gamercoder215.superadvancements.advancement.*;
import me.gamercoder215.superadvancements.advancement.criteria.ACriteria;
import me.gamercoder215.superadvancements.advancement.criteria.trigger.ATrigger;
import me.gamercoder215.superadvancements.advancement.criteria.trigger.ATriggerPredicate;
import me.gamercoder215.superadvancements.advancement.criteria.trigger.DamageTag;
import me.gamercoder215.superadvancements.util.Range;
import me.gamercoder215.superadvancements.wrapper.Wrapper;
import net.minecraft.advancements.*;
import net.minecraft.advancements.critereon.*;
import net.minecraft.commands.CommandFunction;
import net.minecraft.core.BlockPos;
import net.minecraft.core.MappedRegistry;
import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.core.registries.Registries;
import net.minecraft.network.chat.Component;
import net.minecraft.network.protocol.game.ClientboundUpdateAdvancementsPacket;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.PlayerAdvancements;
import net.minecraft.server.ServerAdvancementManager;
import net.minecraft.server.dedicated.DedicatedServer;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.tags.TagKey;
import net.minecraft.util.StringRepresentable;
import net.minecraft.world.damagesource.DamageType;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.alchemy.Potion;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.properties.IntegerProperty;
import net.minecraft.world.level.block.state.properties.Property;
import net.minecraft.world.level.storage.loot.LootContext;
import net.minecraft.world.level.storage.loot.LootDataManager;
import net.minecraft.world.level.storage.loot.predicates.LootItemCondition;
import net.minecraft.world.level.storage.loot.predicates.LootItemEntityPropertyCondition;
import org.bukkit.*;
import org.bukkit.block.Biome;
import org.bukkit.block.BlockState;
import org.bukkit.craftbukkit.v1_20_R2.CraftServer;
import org.bukkit.craftbukkit.v1_20_R2.advancement.CraftAdvancement;
import org.bukkit.craftbukkit.v1_20_R2.block.CraftBlockState;
import org.bukkit.craftbukkit.v1_20_R2.block.CraftBlockStates;
import org.bukkit.craftbukkit.v1_20_R2.enchantments.CraftEnchantment;
import org.bukkit.craftbukkit.v1_20_R2.entity.CraftPlayer;
import org.bukkit.craftbukkit.v1_20_R2.inventory.CraftItemStack;
import org.bukkit.craftbukkit.v1_20_R2.util.CraftMagicNumbers;
import org.bukkit.craftbukkit.v1_20_R2.util.CraftNamespacedKey;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Player;
import org.bukkit.inventory.EquipmentSlot;
import org.bukkit.inventory.ItemStack;
import org.bukkit.potion.PotionType;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.*;
import java.util.function.Supplier;
import java.util.stream.Collectors;

@SuppressWarnings({"unchecked", "rawtypes"})
final class Wrapper1_20_R2 implements Wrapper {

    static {
        CraftServer server = (CraftServer) Bukkit.getServer();
        if (server != null) 
            manager = server.getServer().getAdvancements();
        else
            manager = new ServerAdvancementManager(new LootDataManager());
    }

    private static final ServerAdvancementManager manager;

    public static MinMaxBounds.Ints toIntRange(Range r) {
        if (r == null) return MinMaxBounds.Ints.ANY;
        return MinMaxBounds.Ints.between((int) r.getMinimum(), (int) r.getMaximum());
    }

    public static MinMaxBounds.Doubles toDoubleRange(Range r) {
        if (r == null) return MinMaxBounds.Doubles.ANY;
        return MinMaxBounds.Doubles.between(r.getMinimum(), r.getMaximum());
    }

    public static Range fromNMS(MinMaxBounds.Doubles r) {
        if (r == null) return Range.ANY;
        return new Range(r.min().orElse(0.0), r.max().orElse(0.0));
    }

    public static Range fromNMS(MinMaxBounds.Ints r) {
        if (r == null) return Range.ANY;
        return new Range(r.min().orElse(0), r.max().orElse(0));
    }

    public static ServerPlayer toNMS(Player p) {
        return ((CraftPlayer) p).getHandle();
    }

    public static ResourceLocation toNMS(NamespacedKey key) {
        if (key == null) return null;
        return CraftNamespacedKey.toMinecraft(key);
    }

    public static net.minecraft.world.item.ItemStack toNMS(ItemStack item) {
        if (item == null) return null;
        return CraftItemStack.asNMSCopy(item);
    }

    public static Block toNMS(Material m) {
        if (m == null) return null;
        return Block.byItem(CraftItemStack.asNMSCopy(new ItemStack(m)).getItem());
    }

    public static net.minecraft.world.item.enchantment.Enchantment toNMS(Enchantment enchantment) {
        return CraftEnchantment.getRaw(enchantment);
    }

    public static Potion toNMS(PotionType type) {
        if (type == null) return null;
        ResourceLocation loc = new ResourceLocation(type.name().toLowerCase());
        return BuiltInRegistries.POTION.get(loc);
    }

    public static net.minecraft.world.entity.EntityType<?> toNMS(EntityType type) {
        if (type == null) return null;
        ResourceLocation loc = CraftNamespacedKey.toMinecraft(type.getKey());
        return BuiltInRegistries.ENTITY_TYPE.get(loc);
    }

    public static Set<Property.Value> toNMS(BlockState state) {
        if (state == null) return Set.of();

        net.minecraft.world.level.block.state.BlockState nms = ((CraftBlockState) state).getHandle();
        Set<Property.Value> set = new HashSet<>();

        for (Property<? extends Comparable<?>> p : nms.getBlock().getStateDefinition().getProperties())
            set.add(new Property.Value(p, nms.getValue(p)));

        return set;
    }

    @SuppressWarnings("RedundantCast")
    public static Optional<StatePropertiesPredicate> toNMSP(BlockState state) {
        if (state == null) return Optional.empty();
        Set<Property.Value> set = toNMS(state);

        StatePropertiesPredicate.Builder builder = StatePropertiesPredicate.Builder.properties();

        // Gradle will not compile without casting
        set.forEach(v -> builder.hasProperty(v.property(), (Comparable & StringRepresentable) v.value()));

        return builder.build();
    }

    public static TagKey<DamageType> toNMS(DamageTag tag) {
        if (tag == null) return null;
        return TagKey.create(Registries.DAMAGE_TYPE, new ResourceLocation(tag.getKey().getKey()));
    }

    public static <T> MappedRegistry<T> getRegistry(ResourceKey<Registry<T>> key) {
        DedicatedServer server = ((CraftServer) Bukkit.getServer()).getServer();
        return (MappedRegistry<T>) server.registryAccess().registryOrThrow(key);
    }

    public static <T> ResourceKey<T> getKey(Keyed keyed, ResourceKey<Registry<T>> registry) {
        if (keyed == null) return null;
        ResourceLocation loc = CraftNamespacedKey.toMinecraft(keyed.getKey());
        return ResourceKey.create(registry, loc);
    }

    public static LightPredicate.Builder toNMS(ATriggerPredicate.Light predicate) {
        if (predicate == null) return LightPredicate.Builder.light();
        return new LightPredicate.Builder()
                .setComposite(toIntRange(predicate.getBrightnessRange()));
    }

    public static BlockPredicate.Builder toNMS(ATriggerPredicate.Block predicate) {
        if (predicate == null) return BlockPredicate.Builder.block();
        return BlockPredicate.Builder.block()
                .of(predicate.getMaterials().stream()
                        .map(Wrapper1_20_R2::toNMS)
                        .toArray(Block[]::new)
                );
    }

    public static LocationPredicate.Builder toNMS(ATriggerPredicate.Location predicate) {
        if (predicate == null) return LocationPredicate.Builder.location();
        LocationPredicate.Builder builder = new LocationPredicate.Builder()
                .setX(toDoubleRange(predicate.getXRange()))
                .setY(toDoubleRange(predicate.getYRange()))
                .setZ(toDoubleRange(predicate.getZRange()))
                .setSmokey(predicate.isSmokey());

        if (predicate.getBiome() != null) builder.setBiome(getKey(predicate.getBiome(), Registries.BIOME));
        if (predicate.getBlockPredicate() != null) builder.setBlock(toNMS(predicate.getBlockPredicate()));
        if (predicate.getLightPredicate() != null) builder.setLight(toNMS(predicate.getLightPredicate()));
        if (predicate.getDimension() != null) builder.setDimension(getKey(predicate.getDimension(), Registries.DIMENSION));

        return builder;
    }

    public static Optional<EnchantmentPredicate> toNMS(ATriggerPredicate.Enchantment predicate) {
        if (predicate == null) return Optional.empty();
        return Optional.of(new EnchantmentPredicate(
                toNMS(predicate.getEnchantment()),
                toIntRange(predicate.getLevelRange())
        ));
    }

    public static ItemPredicate.Builder toNMS(ATriggerPredicate.Item predicate) {
        if (predicate == null) return ItemPredicate.Builder.item();
        ItemPredicate.Builder builder = ItemPredicate.Builder.item()
                .of(predicate.getIncludes().stream()
                        .map(CraftItemStack::asNMSCopy)
                        .map(net.minecraft.world.item.ItemStack::getItem)
                        .toArray(Item[]::new))
                .withCount(toIntRange(predicate.getCountRange()))
                .hasDurability(toIntRange(predicate.getDurabilityRange()));

        predicate.getEnchantments().stream()
                .map(Wrapper1_20_R2::toNMS)
                .filter(Optional::isPresent)
                .map(Optional::get)
                .forEach(builder::hasEnchantment);
        predicate.getStoredEnchantments().stream()
                .map(Wrapper1_20_R2::toNMS)
                .filter(Optional::isPresent)
                .map(Optional::get)
                .forEach(builder::hasStoredEnchantment);

        return builder;
    }

    public static EntityPredicate.Builder toNMS(ATriggerPredicate.Entity predicate) {
        if (predicate == null) return EntityPredicate.Builder.entity();

        EntityPredicate.Builder builder = EntityPredicate.Builder.entity();

        if (predicate.getType() != null) builder.entityType(EntityTypePredicate.of(toNMS(predicate.getType())));
        if (predicate.getPlayerDistanceRange() != null) builder.distance(DistancePredicate.absolute(toDoubleRange(predicate.getPlayerDistanceRange())));
        if (!predicate.getEquipment().isEmpty()) {
            EntityEquipmentPredicate.Builder equipment = EntityEquipmentPredicate.Builder.equipment();
            for (Map.Entry<EquipmentSlot, ATriggerPredicate.Item> entry : predicate.getEquipment().entrySet()) {
                switch (entry.getKey()) {
                    case HEAD -> equipment.head(toNMS(entry.getValue()));
                    case CHEST -> equipment.chest(toNMS(entry.getValue()));
                    case LEGS -> equipment.legs(toNMS(entry.getValue()));
                    case FEET -> equipment.feet(toNMS(entry.getValue()));
                    case HAND -> equipment.mainhand(toNMS(entry.getValue()));
                    case OFF_HAND -> equipment.offhand(toNMS(entry.getValue()));
                }
            }

            builder.equipment(equipment.build());
        }

        builder.flags(EntityFlagsPredicate.Builder.flags()
                .setCrouching(predicate.isCrouching())
                .setOnFire(predicate.isOnFire())
                .setSprinting(predicate.isSprinting())
                .setSwimming(predicate.isSwimming())
                .setIsBaby(predicate.isBaby())
        );

        if (predicate.getTarget() != null) builder.targetedEntity(toNMS(predicate.getTarget()));
        if (predicate.getPassenger() != null) builder.passenger(toNMS(predicate.getPassenger()));
        if (predicate.getVehicle() != null) builder.vehicle(toNMS(predicate.getVehicle()));

        return builder;
    }

    public static DamagePredicate toNMS(ATriggerPredicate.Damage predicate) {
        if (predicate == null) return DamagePredicate.Builder.damageInstance().build();

        DamagePredicate.Builder builder = DamagePredicate.Builder.damageInstance();
        builder.dealtDamage(toDoubleRange(predicate.getDealtRange()));
        builder.takenDamage(toDoubleRange(predicate.getTakenRange()));
        builder.blocked(predicate.wasBlocked());

        if (predicate.getSourcePredicate() != null) builder.sourceEntity(toNMSP(predicate.getSourcePredicate()));
        if (predicate.getCause() != null)
            builder.type(DamageSourcePredicate.Builder.damageType().tag(TagPredicate.is(toNMS(predicate.getCause()))));

        return builder.build();
    }

    public static EntityPredicate toNMSP(ATriggerPredicate.Entity predicate) {
        if (predicate == null) return EntityPredicate.Builder.entity().build();
        return toNMS(predicate).build();
    }

    public static ItemPredicate toNMSP(ATriggerPredicate.Item predicate) {
        if (predicate == null) return ItemPredicate.Builder.item().build();
        return toNMS(predicate).build();
    }

    public static LocationPredicate toNMSP(ATriggerPredicate.Location predicate) {
        if (predicate == null) return LocationPredicate.Builder.location().build();
        return toNMS(predicate).build();
    }

    public static Optional<DamageSourcePredicate> toNMSP(DamageTag tag) {
        if (tag == null) return Optional.of(DamageSourcePredicate.Builder.damageType().build());
        return Optional.of(DamageSourcePredicate.Builder.damageType().tag(TagPredicate.is(toNMS(tag))).build());
    }

    public static DistancePredicate toNMSP(Range range) {
        if (range == null) return new DistancePredicate(MinMaxBounds.Doubles.ANY, MinMaxBounds.Doubles.ANY, MinMaxBounds.Doubles.ANY, MinMaxBounds.Doubles.ANY, MinMaxBounds.Doubles.ANY);
        return DistancePredicate.absolute(toDoubleRange(range));
    }
    
    public static Optional<ContextAwarePredicate> wrap(EntityPredicate predicate) {
        if (predicate == EntityPredicate.Builder.entity().build()) return Optional.of(ContextAwarePredicate.create());
        
        LootItemCondition contiion = LootItemEntityPropertyCondition.hasProperties(LootContext.EntityTarget.THIS, predicate).build();
        return Optional.of(ContextAwarePredicate.create(contiion));
    }

    public static Criterion<?> toNMS(ATrigger trigger) {
        if (trigger == null) return null;

        Map<String, Object> c = trigger.getConditions();
        return switch (trigger.getKey().getKey()) {
            case "impossible" -> CriteriaTriggers.IMPOSSIBLE.createCriterion(new ImpossibleTrigger.TriggerInstance());
            case "allay_drop_item_on_block" -> {
                ATriggerPredicate.Location location = (ATriggerPredicate.Location) c.get("location");
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");

                yield ItemUsedOnLocationTrigger.TriggerInstance.allayDropItemOnBlock(toNMS(location), toNMS(item));
            }
            case "avoid_vibration" -> PlayerTrigger.TriggerInstance.avoidVibration();
            case "bee_nest_destroyed" -> {
                Material block = (Material) c.get("block");
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                Range beesInside = (Range) c.get("num_bees_inside");

                yield BeeNestDestroyedTrigger.TriggerInstance.destroyedBeeNest(toNMS(block), toNMS(item), toIntRange(beesInside));
            }
            case "bred_animals" -> {
                ATriggerPredicate.Entity child = (ATriggerPredicate.Entity) c.get("child");
                ATriggerPredicate.Entity parent1 = (ATriggerPredicate.Entity) c.get("parent");
                ATriggerPredicate.Entity parent2 = (ATriggerPredicate.Entity) c.get("partner");

                yield BredAnimalsTrigger.TriggerInstance.bredAnimals(
                        Optional.of(toNMSP(parent1)), 
                        Optional.of(toNMSP(parent2)), 
                        Optional.of(toNMSP(child))
                );
            }
            case "brewed_potion" -> {
                PotionType potion = (PotionType) c.get("potion");
                yield CriteriaTriggers.BREWED_POTION.createCriterion(
                        new BrewedPotionTrigger.TriggerInstance(Optional.of(ContextAwarePredicate.create()), toNMS(potion))
                );
            }
            case "changed_dimension" -> {
                World from = (World) c.get("from");
                World to = (World) c.get("to");
                yield ChangeDimensionTrigger.TriggerInstance.changedDimension(getKey(from, Registries.DIMENSION), getKey(to, Registries.DIMENSION));
            }
            case "channeled_lightning" -> {
                ATriggerPredicate.Entity[] victims = (ATriggerPredicate.Entity[]) c.get("victims");
                if (victims == null) yield ChanneledLightningTrigger.TriggerInstance.channeledLightning();

                yield ChanneledLightningTrigger.TriggerInstance.channeledLightning(
                        Arrays.stream(victims)
                                .map(Wrapper1_20_R2::toNMS)
                                .toArray(EntityPredicate.Builder[]::new)
                );
            }
            case "construct_beacon" -> {
                Range level = (Range) c.get("level");
                yield ConstructBeaconTrigger.TriggerInstance.constructedBeacon(toIntRange(level));
            }
            case "consume_item" -> {
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                yield ConsumeItemTrigger.TriggerInstance.usedItem(toNMS(item));
            }
            case "cured_zombie_villager" -> {
                ATriggerPredicate.Entity villager = (ATriggerPredicate.Entity) c.get("villager");
                ATriggerPredicate.Entity zombie = (ATriggerPredicate.Entity) c.get("zombie");
                yield CriteriaTriggers.CURED_ZOMBIE_VILLAGER.createCriterion(
                        new CuredZombieVillagerTrigger.TriggerInstance(Optional.of(ContextAwarePredicate.create()), wrap(toNMSP(zombie)), wrap(toNMSP(villager)))
                );
            }
            case "enchanted_item" -> {
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                Range levels = (Range) c.get("levels");
                yield CriteriaTriggers.ENCHANTED_ITEM.createCriterion(new EnchantedItemTrigger.TriggerInstance(
                        Optional.of(ContextAwarePredicate.create()),
                        Optional.of(toNMSP(item)),
                        toIntRange(levels)
                ));
            }
            case "enter_block" -> {
                Material block = (Material) c.get("block");
                BlockState state = (BlockState) c.get("state");
                yield CriteriaTriggers.ENTER_BLOCK.createCriterion(
                        new EnterBlockTrigger.TriggerInstance(Optional.empty(), toNMS(block), toNMSP(state))
                );
            }
            case "entity_hurt_player" -> {
                ATriggerPredicate.Damage damage = (ATriggerPredicate.Damage) c.get("damage");
                yield EntityHurtPlayerTrigger.TriggerInstance.entityHurtPlayer(toNMS(damage));
            }
            case "entity_killed_player" -> {
                ATriggerPredicate.Entity entity = (ATriggerPredicate.Entity) c.get("entity");
                DamageTag killingBlow = (DamageTag) c.get("killing_blow");
                yield KilledTrigger.TriggerInstance.entityKilledPlayer(
                        Optional.of(toNMSP(entity)),
                        toNMSP(killingBlow)
                );
            }
            case "fall_from_height" -> {
                ATriggerPredicate.Location start = (ATriggerPredicate.Location) c.get("start_position");
                Range distance = (Range) c.get("distance");
                yield DistanceTrigger.TriggerInstance.fallFromHeight(toNMS(ATriggerPredicate.Entity.ANY), toNMSP(distance), toNMS(start));
            }
            case "fishing_rod_hooked" -> {
                ATriggerPredicate.Item rod = (ATriggerPredicate.Item) c.get("rod");
                ATriggerPredicate.Entity entity = (ATriggerPredicate.Entity) c.get("entity");
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                yield FishingRodHookedTrigger.TriggerInstance.fishedItem(
                        Optional.of(toNMSP(rod)),
                        Optional.of(toNMSP(entity)),
                        Optional.of(toNMSP(item))
                );
            }
            case "hero_of_the_village" -> PlayerTrigger.TriggerInstance.raidWon();
            case "inventory_changed" -> {
                Range emptySlots = (Range) c.get("empty_slots");
                Range fullSlots = (Range) c.get("full_slots");
                Range occupiedSlots = (Range) c.get("occupied_slots");
                Set<ATriggerPredicate.Item> items = (Set<ATriggerPredicate.Item>) c.get("items");

                List<ItemPredicate> predicates = new ArrayList<>();
                if (items != null)
                    predicates.addAll(items.stream()
                            .filter(Objects::nonNull)
                            .map(Wrapper1_20_R2::toNMSP)
                            .toList());

                yield CriteriaTriggers.INVENTORY_CHANGED.createCriterion(
                        new InventoryChangeTrigger.TriggerInstance(Optional.of(ContextAwarePredicate.create()), toIntRange(emptySlots), toIntRange(fullSlots), toIntRange(occupiedSlots), predicates)
                );
            }
            case "item_durability_changed" -> {
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                Range delta = (Range) c.get("delta");
                Range durability = (Range) c.get("durability");
                yield CriteriaTriggers.ITEM_DURABILITY_CHANGED.createCriterion(
                        new ItemDurabilityTrigger.TriggerInstance(Optional.of(ContextAwarePredicate.create()), Optional.of(toNMSP(item)), toIntRange(delta), toIntRange(durability))
                );
            }
            case "item_used_on_block" -> {
                ATriggerPredicate.Location location = (ATriggerPredicate.Location) c.get("location");
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                yield ItemUsedOnLocationTrigger.TriggerInstance.itemUsedOnBlock(toNMS(location), toNMS(item));
            }
            case "kill_mob_near_sculk_catalyst" -> {
                ATriggerPredicate.Entity entity = (ATriggerPredicate.Entity) c.get("entity");
                DamageTag killingBlow = (DamageTag) c.get("killing_blow");
                yield CriteriaTriggers.KILL_MOB_NEAR_SCULK_CATALYST.createCriterion(
                        new KilledTrigger.TriggerInstance(Optional.of(ContextAwarePredicate.create()), wrap(toNMSP(entity)), toNMSP(killingBlow))
                );
            }
            case "killed_by_crossbow" -> {
                Range uniqueTypes = (Range) c.get("unique_entity_types");
                Set<ATriggerPredicate.Entity> victims = (Set<ATriggerPredicate.Entity>) c.get("victims");

                List<ContextAwarePredicate> predicates = new ArrayList<>();
                if (victims != null)
                    predicates.addAll(victims.stream()
                            .filter(Objects::nonNull)
                            .map(Wrapper1_20_R2::toNMSP)
                            .map(Wrapper1_20_R2::wrap)
                            .filter(Optional::isPresent)
                            .map(Optional::get)
                            .toList());

                yield CriteriaTriggers.KILLED_BY_CROSSBOW.createCriterion(
                        new KilledByCrossbowTrigger.TriggerInstance(Optional.of(ContextAwarePredicate.create()), predicates, toIntRange(uniqueTypes))
                );
            }
            case "levitation" -> {
                Range distance = (Range) c.get("distance");
                Range duration = (Range) c.get("duration");
                yield CriteriaTriggers.LEVITATION.createCriterion(
                        new LevitationTrigger.TriggerInstance(Optional.of(ContextAwarePredicate.create()), Optional.of(toNMSP(distance)), toIntRange(duration))
                );
            }
            case "lightning_strike" -> {
                ATriggerPredicate.Entity lightning = (ATriggerPredicate.Entity) c.get("lightning");
                ATriggerPredicate.Entity bystander = (ATriggerPredicate.Entity) c.get("bystander");
                yield LightningStrikeTrigger.TriggerInstance.lightningStrike(
                        Optional.of(toNMSP(lightning)),
                        Optional.of(toNMSP(bystander))
                );
            }
            case "location" -> CriteriaTriggers.LOCATION.createCriterion(
                    new PlayerTrigger.TriggerInstance(Optional.of(ContextAwarePredicate.create()))
            );
            case "nether_travel" -> {
                ATriggerPredicate.Location startPosition = (ATriggerPredicate.Location) c.get("start_position");
                Range distance = (Range) c.get("distance");
                yield CriteriaTriggers.NETHER_TRAVEL.createCriterion(
                        new DistanceTrigger.TriggerInstance(Optional.of(ContextAwarePredicate.create()),
                                Optional.of(toNMSP(startPosition)),
                                Optional.of(toNMSP(distance))
                        )
                );
            }
            case "placed_block" -> {
                Material block = (Material) c.get("block");
                yield ItemUsedOnLocationTrigger.TriggerInstance.placedBlock(toNMS(block));
            }
            case "player_generates_container_loot" -> {
                NamespacedKey lootTable = (NamespacedKey) c.get("loot_table");
                yield LootTableTrigger.TriggerInstance.lootTableUsed(toNMS(lootTable));
            }
            case "player_hurt_entity" -> {
                ATriggerPredicate.Damage damage = (ATriggerPredicate.Damage) c.get("damage");
                ATriggerPredicate.Entity entity = (ATriggerPredicate.Entity) c.get("entity");
                yield PlayerHurtEntityTrigger.TriggerInstance.playerHurtEntity(
                        Optional.of(toNMS(damage)),
                        Optional.of(toNMSP(entity))
                );
            }
            case "player_interacted_with_entity" -> {
                ATriggerPredicate.Entity entity = (ATriggerPredicate.Entity) c.get("entity");
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                yield PlayerInteractTrigger.TriggerInstance.itemUsedOnEntity(toNMS(item), wrap(toNMSP(entity)));
            }
            case "player_killed_entity" -> {
                ATriggerPredicate.Entity entity = (ATriggerPredicate.Entity) c.get("entity");
                DamageTag killingBlow = (DamageTag) c.get("killing_blow");
                yield KilledTrigger.TriggerInstance.playerKilledEntity(
                        Optional.of(toNMSP(entity)),
                        toNMSP(killingBlow)
                );
            }
            case "recipe_unlocked" -> {
                NamespacedKey recipe = (NamespacedKey) c.get("recipe");
                yield CriteriaTriggers.RECIPE_UNLOCKED.createCriterion(
                        new RecipeUnlockedTrigger.TriggerInstance(Optional.of(ContextAwarePredicate.create()), toNMS(recipe))
                );
            }
            case "ride_entity_in_lava" -> {
                ATriggerPredicate.Location startPosition = (ATriggerPredicate.Location) c.get("start_position");
                Range distance = (Range) c.get("distance");
                yield CriteriaTriggers.RIDE_ENTITY_IN_LAVA_TRIGGER.createCriterion(
                        new DistanceTrigger.TriggerInstance(
                                Optional.of(ContextAwarePredicate.create()),
                                Optional.of(toNMSP(startPosition)),
                                Optional.of(toNMSP(distance))
                        )
                );
            }
            case "shot_crossbow" -> {
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                yield ShotCrossbowTrigger.TriggerInstance.shotCrossbow(Optional.of(toNMSP(item)));
            }
            case "slept_in_bed" -> PlayerTrigger.TriggerInstance.sleptInBed();
            case "slide_down_block" -> {
                Material block = (Material) c.get("block");
                BlockState state = (BlockState) c.get("state");
                yield CriteriaTriggers.HONEY_BLOCK_SLIDE.createCriterion(
                        new SlideDownBlockTrigger.TriggerInstance(Optional.of(ContextAwarePredicate.create()), toNMS(block), toNMSP(state))
                );
            }
            case "started_riding" -> CriteriaTriggers.START_RIDING_TRIGGER.createCriterion(
                    new StartRidingTrigger.TriggerInstance(Optional.of(ContextAwarePredicate.create()))
            );
            case "tame_animal" -> {
                ATriggerPredicate.Entity entity = (ATriggerPredicate.Entity) c.get("entity");
                yield TameAnimalTrigger.TriggerInstance.tamedAnimal(toNMS(entity));
            }
            case "target_hit" -> {
                Range signalStrength = (Range) c.get("signal_strength");
                ATriggerPredicate.Entity projectile = (ATriggerPredicate.Entity) c.get("projectile");
                yield TargetBlockTrigger.TriggerInstance.targetHit(toIntRange(signalStrength), wrap(toNMSP(projectile)));
            }
            case "thrown_item_picked_up_by_entity" -> {
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                ATriggerPredicate.Entity entity = (ATriggerPredicate.Entity) c.get("entity");

                yield CriteriaTriggers.THROWN_ITEM_PICKED_UP_BY_ENTITY.createCriterion(new PickedUpItemTrigger.TriggerInstance(
                        Optional.of(ContextAwarePredicate.create()),
                        Optional.of(toNMSP(item)),
                        wrap(toNMSP(entity))
                ));
            }
            case "thrown_item_picked_up_by_player" -> {
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                ATriggerPredicate.Entity entity = (ATriggerPredicate.Entity) c.get("entity");

                yield CriteriaTriggers.THROWN_ITEM_PICKED_UP_BY_PLAYER.createCriterion(new PickedUpItemTrigger.TriggerInstance(
                        Optional.of(ContextAwarePredicate.create()),
                        Optional.of(toNMSP(item)),
                        wrap(toNMSP(entity))
                ));
            }
            case "tick" -> PlayerTrigger.TriggerInstance.tick();
            case "used_ender_eye" -> {
                Range distance = (Range) c.get("distance");
                yield CriteriaTriggers.USED_ENDER_EYE.createCriterion(
                        new UsedEnderEyeTrigger.TriggerInstance(Optional.of(ContextAwarePredicate.create()), toDoubleRange(distance))
                );
            }
            case "used_totem" -> {
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                yield UsedTotemTrigger.TriggerInstance.usedTotem(toNMSP(item));
            }
            case "using_item" -> {
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                yield CriteriaTriggers.USING_ITEM.createCriterion(
                        new UsingItemTrigger.TriggerInstance(Optional.of(ContextAwarePredicate.create()), Optional.of(toNMSP(item)))
                );
            }
            case "villager_trade" -> {
                ATriggerPredicate.Item item = (ATriggerPredicate.Item) c.get("item");
                ATriggerPredicate.Entity villager = (ATriggerPredicate.Entity) c.get("villager");
                yield CriteriaTriggers.TRADE.createCriterion(
                        new TradeTrigger.TriggerInstance(Optional.of(ContextAwarePredicate.create()), wrap(toNMSP(villager)), Optional.of(toNMSP(item)))
                );
            }
            case "voluntary_exile" -> CriteriaTriggers.BAD_OMEN.createCriterion(
                    new PlayerTrigger.TriggerInstance(Optional.of(ContextAwarePredicate.create()))
            );
            default -> throw new IllegalArgumentException("Unknown Advancement Trigger: " + trigger);
        };
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

    public static ACriteria fromNMS(Criterion<?> t) {
        if (t == null) return null;

        return new ACriteria(switch (CriteriaTriggers.getId(t.trigger()).getPath()) {
            case "impossible" -> ATrigger.impossible();
            case "allay_drop_item_on_block" -> ATrigger.allayDropItemOnBlock(
                fromNMS(getObject(t, "a", LocationPredicate.class)),
                fromNMS(getObject(t, "b", ItemPredicate.class))
            );
            case "avoid_vibration" -> ATrigger.avoidVibration();
            case "bee_nest_destroyed" -> ATrigger.beeNestDestroyed(
                fromNMS(getObject(t, "a", Block.class)),
                fromNMS(getObject(t, "b", ItemPredicate.class)),
                fromNMS(getObject(t, "c", MinMaxBounds.Ints.class))
            );
            case "bred_animals" -> ATrigger.bredAnimals(
                fromNMS(getObject(t, "c", ContextAwarePredicate.class)),
                fromNMS(getObject(t, "a", ContextAwarePredicate.class)),
                fromNMS(getObject(t, "b", ContextAwarePredicate.class))
            );
            case "brewed_potion" -> ATrigger.brewedPotion(
                fromNMS(getObject(t, "a", Potion.class))
            );
            case "changed_dimension" -> ATrigger.changedDimension(
                fromNMSW(getObject(t, "a", ResourceKey.class)),
                fromNMSW(getObject(t, "b", ResourceKey.class))
            );
            case "channeled_lightning" -> ATrigger.channeledLightning(
                Arrays.stream(getObject(t, "a", ContextAwarePredicate[].class))
                        .map(Wrapper1_20_R2::fromNMS)
                        .collect(Collectors.toSet())
            );
            case "construct_beacon" -> ATrigger.constructBeacon(
                fromNMS(getObject(t, "a", MinMaxBounds.Ints.class))
            );
            case "consume_item" -> ATrigger.consumeItem(
                fromNMS(getObject(t, "a", ItemPredicate.class))
            );
            case "cured_zombie_villager" -> ATrigger.curedZombieVillager(
                fromNMS(getObject(t, "a", ContextAwarePredicate.class)),
                fromNMS(getObject(t, "b", ContextAwarePredicate.class))
            );
            case "enchanted_item" -> ATrigger.enchantedItem(
                fromNMS(getObject(t, "a", ItemPredicate.class)),
                fromNMS(getObject(t, "b", MinMaxBounds.Ints.class))
            );
            case "enter_block" -> ATrigger.enterBlock(
                fromNMS(getObject(t, "a", Block.class)),
                fromNMS(getObject(t, "a", Block.class), getObject(t, "b", StatePropertiesPredicate.class))
            );
            case "entity_hurt_player" -> ATrigger.entityHurtPlayer(
                fromNMS(getObject(t, "a", DamagePredicate.class))
            );
            case "entity_killed_player" -> ATrigger.entityKilledPlayer(
                fromNMS(getObject(t, "a", ContextAwarePredicate.class)),
                fromNMS(getObject(t, "b", DamageSourcePredicate.class))
            );
            case "fall_from_height" -> ATrigger.fallFromHeight(
                fromNMS(getObject(t, "b", DistancePredicate.class)),
                fromNMS(getObject(t, "a", LocationPredicate.class))
            );
            case "fishing_rod_hooked" -> ATrigger.fishingRodHooked(
                fromNMS(getObject(t, "a", ItemPredicate.class)),
                fromNMS(getObject(t, "b", EntityPredicate.class)),
                fromNMS(getObject(t, "c", ItemPredicate.class))
            );
            case "hero_of_the_village" -> ATrigger.heroOfTheVillage();
            case "inventory_changed" -> ATrigger.inventoryChanged(
                fromNMS(getObject(t, "c", MinMaxBounds.Ints.class)),
                fromNMS(getObject(t, "b", MinMaxBounds.Ints.class)),
                fromNMS(getObject(t, "a", MinMaxBounds.Ints.class)),
                Arrays.stream(getObject(t, "d", ItemPredicate[].class))
                        .map(Wrapper1_20_R2::fromNMS)
                        .collect(Collectors.toSet())
            );
            case "item_durability_changed" -> ATrigger.itemDurabilityChanged(
                fromNMS(getObject(t, "a", ItemPredicate.class)),
                fromNMS(getObject(t, "c", MinMaxBounds.Ints.class)),
                fromNMS(getObject(t, "b", MinMaxBounds.Ints.class))
            );
            case "item_used_on_block" -> ATrigger.itemUsedOnBlock(
                fromNMS(getObject(t, "a", LocationPredicate.class)),
                fromNMS(getObject(t, "b", ItemPredicate.class))
            );
            case "kill_mob_near_sculk_catalyst" -> ATrigger.killMobNearSculkCatalyst(
                fromNMS(getObject(t, "a", ContextAwarePredicate.class)),
                fromNMS(getObject(t, "b", DamageSourcePredicate.class))
            );
            case "killed_by_crossbow" -> ATrigger.killedByCrossbow(
                fromNMS(getObject(t, "b", MinMaxBounds.Ints.class)),
                Arrays.stream(getObject(t, "a", ContextAwarePredicate[].class))
                        .map(Wrapper1_20_R2::fromNMS)
                        .collect(Collectors.toSet())
            );
            case "levitation" -> ATrigger.levitation(
                fromNMS(getObject(t, "a", DistancePredicate.class)),
                fromNMS(getObject(t, "b", MinMaxBounds.Ints.class))
            );
            case "location" -> ATrigger.location();
            case "nether_travel" -> ATrigger.netherTravel(
                fromNMS(getObject(t, "a", LocationPredicate.class)),
                fromNMS(getObject(t, "b", DistancePredicate.class))
            );
            case "placed_block" -> ATrigger.placedBlock(
                fromNMS(getObject(t, "a", Block.class)),
                fromNMS(getObject(t, "d", ItemPredicate.class)),
                fromNMS(getObject(t, "c", LocationPredicate.class)),
                fromNMS(getObject(t, "a", Block.class), getObject(t, "b", StatePropertiesPredicate.class))
            );
            case "player_generates_container_loot" -> ATrigger.playerGeneratesContainerLoot(
                fromNMS(getObject(t, "a", ResourceLocation.class))
            );
            case "player_hurt_entity" -> ATrigger.playerHurtEntity(
                fromNMS(getObject(t, "a", DamagePredicate.class)),
                fromNMS(getObject(t, "b", ContextAwarePredicate.class))
            );
            case "player_interacted_with_entity" -> ATrigger.playerInteractedWithEntity(
                fromNMS(getObject(t, "b", ContextAwarePredicate.class)),
                fromNMS(getObject(t, "a", ItemPredicate.class))
            );
            case "player_killed_entity" -> ATrigger.playerKilledEntity(
                fromNMS(getObject(t, "a", ContextAwarePredicate.class)),
                fromNMS(getObject(t, "b", DamageSourcePredicate.class))
            );
            case "recipe_unlocked" -> ATrigger.recipeUnlocked(
                fromNMS(getObject(t, "a", ResourceLocation.class))
            );
            case "ride_entity_in_lava" -> ATrigger.rideEntityInLava(
                fromNMS(getObject(t, "a", LocationPredicate.class)),
                fromNMS(getObject(t, "b", DistancePredicate.class))
            );
            case "shot_crossbow" -> ATrigger.shotCrossbow(
                fromNMS(getObject(t, "a", ItemPredicate.class))
            );
            case "slept_in_bed" -> ATrigger.sleptInBed();
            case "slide_down_block" -> ATrigger.slideDownBlock(
                fromNMS(getObject(t, "a", Block.class)),
                fromNMS(getObject(t, "a", Block.class), getObject(t, "b", StatePropertiesPredicate.class))
            );
            case "started_riding" -> ATrigger.startedRiding();
            case "tame_animal" -> ATrigger.tameAnimal(
                fromNMS(getObject(t, "a", ContextAwarePredicate.class))
            );
            case "target_hit" -> ATrigger.targetHit(
                fromNMS(getObject(t, "a", MinMaxBounds.Ints.class)),
                fromNMS(getObject(t, "b", ContextAwarePredicate.class))
            );
            case "thrown_item_picked_up_by_entity" -> ATrigger.thrownItemPickedUpByEntity(
                fromNMS(getObject(t, "a", ItemPredicate.class)),
                fromNMS(getObject(t, "b", ContextAwarePredicate.class))
            );
            case "thrown_item_picked_up_by_player" -> ATrigger.thrownItemPickedUpByPlayer(
                fromNMS(getObject(t, "a", ItemPredicate.class)),
                fromNMS(getObject(t, "b", ContextAwarePredicate.class))
            );
            case "tick" -> ATrigger.tick();
            case "used_ender_eye" -> ATrigger.usedEnderEye(
                fromNMS(getObject(t, "a", MinMaxBounds.Doubles.class))
            );
            case "used_totem" -> ATrigger.usedTotem(
                fromNMS(getObject(t, "a", ItemPredicate.class))
            );
            case "using_item" -> ATrigger.usingItem(
                fromNMS(getObject(t, "a", ItemPredicate.class))
            );
            case "villager_trade" -> ATrigger.villagerTrade(
                fromNMS(getObject(t, "b", ItemPredicate.class)),
                fromNMS(getObject(t, "a", ContextAwarePredicate.class))
            );
            case "voluntary_exile" -> ATrigger.voluntaryExile();
            default -> throw new IllegalArgumentException("Unknown Advancement Trigger: " + t.trigger());
        });
    }

    public static Criterion toNMS(ACriteria criteria) {
        if (criteria == null) return null;
        return toNMS(criteria.getTrigger());
    }

    public static AdvancementRewards toNMS(AReward reward) {
        if (reward == null) return AdvancementRewards.EMPTY;
        return new AdvancementRewards(
                reward.getExperience(),
                reward.getLootTables() == null ? null : reward.getLootTables().stream()
                        .map(Wrapper1_20_R2::toNMS)
                        .toArray(ResourceLocation[]::new),
                reward.getRecipes() == null ? null : reward.getRecipes().stream()
                        .map(Keyed::getKey)
                        .map(Wrapper1_20_R2::toNMS)
                        .toArray(ResourceLocation[]::new),
                CommandFunction.CacheableFunction.NONE
        );
    }

    public static net.minecraft.advancements.AdvancementHolder toNMS(Advancement a) {
        if (a == null) return null;
        if (manager.advancements.get(toNMS(a.getKey())) != null) return manager.advancements.get(toNMS(a.getKey()));

        ADisplay display = a.getDisplay();
        String title = display.getTitleAsString();
        String desc = display.getDescriptionAsString();
        FrameType frame = Arrays.stream(FrameType.values()).filter(f -> f.getName().equalsIgnoreCase(display.getFrame().name())).findFirst().orElse(FrameType.TASK);
        ResourceLocation bg = null;

        if (a.getParent() == null && display.getBackgroundTexture() != null)
            bg = new ResourceLocation(display.getBackgroundTexture());

        DisplayInfo nmsDisplay = new DisplayInfo(toNMS(display.getIcon()), Component.literal(title), Component.literal(desc), bg, frame, a.hasFlag(AFlag.TOAST), a.hasFlag(AFlag.MESSAGE), a.hasFlag(AFlag.HIDDEN));
        nmsDisplay.setLocation(display.getX(), display.getY());

        ResourceLocation parent = a.getParent() == null ? null : toNMS(a.getParent().getKey());

        net.minecraft.advancements.Advancement adv = new net.minecraft.advancements.Advancement(
                Optional.ofNullable(parent),
                Optional.of(nmsDisplay),
                toNMS(a.getReward()),
                a.getCriteria().entrySet().stream()
                        .collect(Collectors.toMap(Map.Entry::getKey, e -> toNMS(e.getValue()))),
                a.hasFlag(AFlag.MERGE_CRITERIA) ? AdvancementRequirements.Strategy.OR.create(a.getCriteria().keySet()) : AdvancementRequirements.Strategy.AND.create(a.getCriteria().keySet()),
                a.isTelemetry()
        );

        return new AdvancementHolder(toNMS(a.getKey()), adv);
    }

    public static ItemStack fromNMS(net.minecraft.world.item.ItemStack item) {
        if (item == null) return null;
        return CraftItemStack.asBukkitCopy(item);
    }

    public static NamespacedKey fromNMS(ResourceLocation key) {
        if (key == null) return null;
        return CraftNamespacedKey.fromMinecraft(key);
    }

    public static Biome fromNMS(ResourceKey<net.minecraft.world.level.biome.Biome> biome) {
        if (biome == null) return null;
        MappedRegistry<net.minecraft.world.level.biome.Biome> registry = getRegistry(Registries.BIOME);
        net.minecraft.world.level.biome.Biome biomeBase = registry.getHolderOrThrow(biome).value();
        Biome bukkit = org.bukkit.Registry.BIOME.get(fromNMS(registry.getKey(biomeBase)));

        return bukkit == null ? Biome.CUSTOM : bukkit;
    }

    public static World fromNMSW(ResourceKey<net.minecraft.world.level.Level> world) {
        if (world == null) return null;
        MappedRegistry<net.minecraft.world.level.Level> registry = getRegistry(Registries.DIMENSION);
        return registry.getOrThrow(world).getWorld();
    }

    public static Material fromNMS(Block block) {
        if (block == null) return null;
        return CraftMagicNumbers.getMaterial(block);
    }

    public static EntityType fromNMS(EntityTypePredicate p) {
        if (p == null) return null;
        try {
            Field typeF = p.getClass().getDeclaredField("b");
            typeF.setAccessible(true);

            Class<?> typeC = typeF.getType();

            if (net.minecraft.world.entity.EntityType.class.isAssignableFrom(typeC)) {
                net.minecraft.world.entity.EntityType<?> entityType = (net.minecraft.world.entity.EntityType<?>) typeF.get(p);
                return EntityType.valueOf(getRegistry(Registries.ENTITY_TYPE).getKey(entityType).getPath().toUpperCase());
            } else if (TagKey.class.isAssignableFrom(typeC)) {
                return null;
            } else {
                throw new IllegalArgumentException("Unknown EntityTypePredicate Field: " + typeC);
            }
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }
    }

    public static DamageTag fromNMS(DamageSourcePredicate predicate) {
        if (predicate == null) return null;
        List<TagPredicate<DamageType>> tags = getObject(predicate, "b", List.class);

        TagPredicate<DamageType> tP = tags.stream()
                .filter(t -> getBoolean(t, "b"))
                .findFirst()
                .orElse(null);

        if (tP == null) return null;

        return fromNMS(getObject(tP, "a", TagKey.class));
    }

    public static Range fromNMS(DistancePredicate p) {
        if (p == null) return null;
        return fromNMS(getObject(p, "f", MinMaxBounds.Doubles.class));
    }

    public static DamageTag fromNMS(TagKey<DamageType> key) {
        if (key == null) return null;
        return DamageTag.valueOf(key.location().getPath().toUpperCase());
    }

    public static ItemStack fromNMS(Item item) {
        if (item == null) return null;
        return CraftItemStack.asBukkitCopy(new net.minecraft.world.item.ItemStack(item));
    }

    public static Enchantment fromNMS(net.minecraft.world.item.enchantment.Enchantment enchant) {
        if (enchant == null) return null;
        NamespacedKey key = fromNMS(BuiltInRegistries.ENCHANTMENT.getKey(enchant));
        return CraftEnchantment.getByKey(key);
    }

    public static PotionType fromNMS(Potion potion) {
        if (potion == null) return null;
        return Arrays.stream(PotionType.values())
                .filter(p -> p.getKey().getKey().equalsIgnoreCase(potion.getName("")))
                .findFirst()
                .orElse(PotionType.WATER);
    }

    public static BlockState fromNMS(Block block, StatePropertiesPredicate predicate) {
        if (predicate == null) return null;
        List<Object> properties = getObject(predicate, "b", List.class);

        try {
            Class<?> propertyMatcherC = Class.forName("net.minecraft.advancements.critereon.CriterionTriggerProperties$c");

            Map<String, String> propertyMap = properties.stream()
            .map(o -> {
                try {
                    Field nameF = propertyMatcherC.getDeclaredField("a");
                    nameF.setAccessible(true);
                    String name = (String) nameF.get(o);

                    if (o.getClass().getDeclaredFields().length == 2) {
                        // private class RangedPropertyMatcher
                        Field minF = o.getClass().getDeclaredField("a");
                        String min = String.valueOf(minF.get(o));

                        return new AbstractMap.SimpleEntry<>(name, min);
                    } else {
                        // private class ExactPropertyMatcher
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

            Map<Property<?>, Comparable<?>> prop = new HashMap<>();

            for (Property<?> property : block.getStateDefinition().getProperties()) {
                if (!propertyMap.containsKey(property.getName())) continue;

                String valueS = propertyMap.get(property.getName());
                if (valueS.equals("true") || valueS.equals("false"))
                    prop.put(property, Boolean.parseBoolean(valueS));
                else if (property.getClass().isEnum())
                    prop.put(property, Enum.valueOf(property.getClass().asSubclass(Enum.class), valueS));
                else if (property instanceof IntegerProperty)
                    prop.put(property, Integer.parseInt(valueS));
                else
                    prop.put(property, valueS);
            }

            Supplier<net.minecraft.world.level.block.state.BlockState> supplier = block::defaultBlockState;

            net.minecraft.world.level.block.state.BlockState nms = new net.minecraft.world.level.block.state.BlockState(
                block,
                ImmutableMap.copyOf(prop),
                MapCodec.of(Encoder.empty(), Decoder.unit(supplier))
            );

            return CraftBlockStates.getBlockState(BlockPos.ZERO, nms, null);
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }
    }

    public static ATriggerPredicate.Entity fromNMS(ContextAwarePredicate predicate) {
        if (predicate == null) return null;
        if (predicate.equals(ContextAwarePredicate.create())) return ATriggerPredicate.Entity.ANY;
        
        LootItemCondition[] conditions = getObject(predicate, "b", LootItemCondition[].class);
        return fromNMS(Arrays.stream(conditions)
                .filter(c -> c instanceof LootItemEntityPropertyCondition)
                .map(c -> getObject(c, "a", EntityPredicate.class))
                .findFirst()
                .orElse(null));
    }

    public static ATriggerPredicate.Damage fromNMS(DamagePredicate p) {
        if (p == null) return null;
        return ATriggerPredicate.Damage.builder()
                .dealt(fromNMS(getObject(p, "b", MinMaxBounds.Doubles.class)))
                .taken(fromNMS(getObject(p, "c", MinMaxBounds.Doubles.class)))
                .source(fromNMS(getObject(p, "d", EntityPredicate.class)))
                .build();
    }

    public static ATriggerPredicate.Entity fromNMS(EntityPredicate p) {
        if (p == null) return null;
        EntityFlagsPredicate flags = getObject(p, "h", EntityFlagsPredicate.class);
        EntityEquipmentPredicate eq = getObject(p, "i", EntityEquipmentPredicate.class);

        return ATriggerPredicate.Entity.builder()
                .type(fromNMS(getObject(p, "b", EntityTypePredicate.class)))
                .distanceToPlayer(fromNMS(getObject(p, "c", DistancePredicate.class)))
                .location(fromNMS(getObject(p, "d", LocationPredicate.class)))
                .steppingLocation(fromNMS(getObject(p, "e", LocationPredicate.class)))
                // Flags
                .onFire(getBoolean(flags, "b"))
                .crouching(getBoolean(flags, "c"))
                .sprinting(getBoolean(flags, "d"))
                .swimming(getBoolean(flags, "e"))
                .baby(getBoolean(flags, "f"))
                // Equipment
                .equipment(Map.of(
                    EquipmentSlot.HEAD, fromNMS(getObject(eq, "c", ItemPredicate.class)),
                    EquipmentSlot.CHEST, fromNMS(getObject(eq, "d", ItemPredicate.class)),
                    EquipmentSlot.LEGS, fromNMS(getObject(eq, "e", ItemPredicate.class)),
                    EquipmentSlot.FEET, fromNMS(getObject(eq, "f", ItemPredicate.class)),
                    EquipmentSlot.HAND, fromNMS(getObject(eq, "g", ItemPredicate.class)),
                    EquipmentSlot.OFF_HAND, fromNMS(getObject(eq, "h", ItemPredicate.class))
                ))
                // Other
                .vehicle(fromNMS(getObject(p, "k", EntityPredicate.class)))
                .passenger(fromNMS(getObject(p, "l", EntityPredicate.class)))
                .target(fromNMS(getObject(p, "m", EntityPredicate.class)))
                .build();
    }

    public static ATriggerPredicate.Enchantment fromNMS(EnchantmentPredicate p) {
        if (p == null) return null;
        return ATriggerPredicate.Enchantment.builder()
                .enchantment(fromNMS(getObject(p, "b", net.minecraft.world.item.enchantment.Enchantment.class)))
                .level(fromNMS(getObject(p, "c", MinMaxBounds.Ints.class)))
                .build();
    }

    public static ATriggerPredicate.Item fromNMS(ItemPredicate p) {
        if (p == null) return null;
        ATriggerPredicate.Item.Builder builder = ATriggerPredicate.Item.builder()
                .include(((Set<Item>) getObject(p, "c", Set.class))
                        .stream()
                        .map(Wrapper1_20_R2::fromNMS)
                        .collect(Collectors.toSet())
                )
                .count(fromNMS(getObject(p, "d", MinMaxBounds.Ints.class)))
                .durability(fromNMS(getObject(p, "e", MinMaxBounds.Ints.class)));

        for (EnchantmentPredicate ench : getObject(p, "f", EnchantmentPredicate[].class))
            builder.enchantment(fromNMS(ench));
        
        for (EnchantmentPredicate ench : getObject(p, "g", EnchantmentPredicate[].class))
            builder.storedEnchantment(fromNMS(ench));

        return builder.build();
    }

    public static ATriggerPredicate.Light fromNMS(LightPredicate p) {
        if (p == null) return null;
        return ATriggerPredicate.Light.of(fromNMS(getObject(p, "b", MinMaxBounds.Ints.class)));
    }

    public static ATriggerPredicate.Block fromNMS(BlockPredicate p) {
        if (p == null) return null;
        return ATriggerPredicate.Block.of(((Set<Block>) getObject(p, "c", Set.class))
                .stream()
                .map(Wrapper1_20_R2::fromNMS)
                .collect(Collectors.toSet())
        );
    }

    public static ATriggerPredicate.Location fromNMS(LocationPredicate p) {
        if (p == null) return null;
        return ATriggerPredicate.Location.builder()
                .x(fromNMS(getObject(p, "c", MinMaxBounds.Doubles.class)))
                .y(fromNMS(getObject(p, "d", MinMaxBounds.Doubles.class)))
                .z(fromNMS(getObject(p, "e", MinMaxBounds.Doubles.class)))
                .biome(fromNMS(getObject(p, "f", ResourceKey.class)))
                .dimension(fromNMSW(getObject(p, "h", ResourceKey.class)))
                .smokey(getBoolean(p, "i"))
                .light(fromNMS(getObject(p, "j", LightPredicate.class)))
                .block(fromNMS(getObject(p, "k", BlockPredicate.class)))
                .build();
    }

    public static AReward fromNMS(AdvancementRewards rewards) {
        if (rewards == null) return AReward.EMPTY;
        try {
            Field experienceF = AdvancementRewards.class.getDeclaredField("b");
            experienceF.setAccessible(true);
            int experience = experienceF.getInt(rewards);

            Field lootF = AdvancementRewards.class.getDeclaredField("c");
            lootF.setAccessible(true);
            ResourceLocation[] loot = (ResourceLocation[]) lootF.get(rewards);

            return new AReward(
                    experience,
                    Arrays.stream(loot).map(Wrapper1_20_R2::fromNMS).collect(Collectors.toList()),
                    Arrays.stream(rewards.getRecipes()).map(Wrapper1_20_R2::fromNMS).collect(Collectors.toList())
            );
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }
    }

    public static Advancement fromNMS(net.minecraft.advancements.AdvancementHolder holder) {
        if (holder == null) return null;
        net.minecraft.advancements.Advancement a = holder.value();

        if (a.display().isEmpty()) return null;
        DisplayInfo nmsDisplay = a.display().get();

        NMSDisplay1_20_R2 display = new NMSDisplay1_20_R2(nmsDisplay);

        Map<String, ACriteria> criteria = a.criteria()
                .entrySet()
                .stream()
                .collect(Collectors.toMap(Map.Entry::getKey, e -> fromNMS(e.getValue())));

        Advancement.Builder builder = Advancement.builder()
                .key(fromNMS(holder.id()))
                .display(display)
                .reward(fromNMS(a.rewards()))
                .criteria(criteria);

        if (a.parent().isPresent()) builder.parent(fromNMS(manager.advancements.get(a.parent().get())));
        if (a.requirements().size() == 1) builder.flags(AFlag.MERGE_CRITERIA);
        if (nmsDisplay.shouldAnnounceChat()) builder.flags(AFlag.MESSAGE);
        if (nmsDisplay.shouldShowToast()) builder.flags(AFlag.TOAST);
        if (nmsDisplay.isHidden()) builder.flags(AFlag.HIDDEN);

        return builder.build();
    }

    // Implementation

    @Override
    public void update(Player p) {
        ServerPlayer sp = toNMS(p);

        sp.getAdvancements().flushDirty(sp);
    }

    @Override
    public void register(Advancement a) {
        ResourceLocation key = toNMS(a.getKey());
        net.minecraft.advancements.AdvancementHolder nms = toNMS(a);

        if (manager.advancements.containsKey(nms.id())) throw new IllegalStateException("Advancement is already registered");
        manager.tree().addAll(Set.of(nms));
    }

    @Override
    public Advancement getAdvancement(NamespacedKey key) {
        net.minecraft.advancements.AdvancementHolder nms = manager.advancements.get(toNMS(key));
        if (nms == null) return null;

        return fromNMS(nms);
    }

    @Override
    public boolean isRegistered(NamespacedKey key) {
        return manager.advancements.containsKey(toNMS(key));
    }

    @Override
    public void unregister(NamespacedKey key) {
        manager.tree().remove(Set.of(toNMS(key)));
        Bukkit.getOnlinePlayers().forEach(p -> removeAdvancement(p, Set.of(key)));
    }

    @Override
    public void addAdvancement(Player p, Set<Advancement> advancements) {
        ServerPlayer sp = toNMS(p);

        Set<net.minecraft.advancements.AdvancementHolder> added = new HashSet<>();
        Map<ResourceLocation, AdvancementProgress> map = new HashMap<>();

        for (Advancement a : advancements) {
            net.minecraft.advancements.AdvancementHolder nms = toNMS(a);
            if (!isRegistered(a.getKey())) register(a);

            try {
                Method regListeners = PlayerAdvancements.class.getDeclaredMethod("d", net.minecraft.advancements.AdvancementHolder.class);
                regListeners.setAccessible(true);
                regListeners.invoke(sp.getAdvancements(), nms);
            } catch (ReflectiveOperationException e) {
                throw new RuntimeException(e);
            }

            AdvancementProgress prog = sp.getAdvancements().getOrStartProgress(nms);
            added.add(nms);
            map.put(nms.id(), prog);
        }

        sp.connection.send(new ClientboundUpdateAdvancementsPacket(false, added, Set.of(), map));
        sp.getAdvancements().flushDirty(sp);
        sp.getAdvancements().save();
    }

    @Override
    public void removeAdvancement(Player p, Set<NamespacedKey> key) {
        ServerPlayer sp = toNMS(p);
        Set<ResourceLocation> removed = key.stream().map(Wrapper1_20_R2::toNMS).collect(Collectors.toSet());
        sp.connection.send(new ClientboundUpdateAdvancementsPacket(false, Set.of(), removed, Map.of()));
    }

    @Override
    public AProgress getProgress(Player p, NamespacedKey key) {
        if (!isRegistered(key)) throw new IllegalArgumentException("Advancement is not registered");
        ServerPlayer sp = toNMS(p);
        net.minecraft.advancements.AdvancementHolder nms = manager.advancements.get(toNMS(key));

        return new AProgress1_20_R2(p, nms, sp.getAdvancements().getOrStartProgress(nms));
    }

    @Override
    public org.bukkit.advancement.Advancement toBukkit(Advancement a) {
        if (a == null) throw new IllegalArgumentException("Advancement cannot be null");
        return toNMS(a).toBukkit();
    }

    @Override
    public Advancement fromBukkit(org.bukkit.advancement.Advancement a) {
        if (a == null) throw new IllegalArgumentException("Advancement cannot be null");

        return fromNMS(((CraftAdvancement) a).getHandle());
    }

    @Override
    public Advancement getSelectedTab(Player p) {
        ServerPlayer sp = toNMS(p);

        net.minecraft.advancements.Advancement lastSelectedTab = getObject(sp.getAdvancements(), "l", net.minecraft.advancements.Advancement.class);
        if (lastSelectedTab == null) return null;

        return fromNMS((manager.advancements.values().stream()
                .filter(a -> a.value().equals(lastSelectedTab))
                .findFirst()
                .orElse(null)));
    }

    @Override
    public void setSelectedTab(Player p, Advancement advancement) {
        Advancement a0 = advancement.getRoot();
        
        if (!isRegistered(a0.getKey())) register(a0);
        ServerPlayer sp = toNMS(p);

        sp.getAdvancements().setSelectedTab(toNMS(a0));
        sp.getAdvancements().flushDirty(sp);
        sp.getAdvancements().save();
    }
}
