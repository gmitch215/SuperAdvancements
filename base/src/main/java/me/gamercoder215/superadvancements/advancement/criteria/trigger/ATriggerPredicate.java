package me.gamercoder215.superadvancements.advancement.criteria.trigger;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import me.gamercoder215.superadvancements.util.Range;
import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.block.Biome;
import org.bukkit.entity.EntityType;
import org.bukkit.inventory.EquipmentSlot;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Represents a set of predicates for a {@link ATrigger} trigger.
 */
public final class ATriggerPredicate {

    private ATriggerPredicate() {}

    /**
     * Represents an Item Predicate.
     */
    public static final class Item {

        /**
         * Represents an Item Predicate that matches any item.
         */
        public static final Item ANY = new Item(EnumSet.allOf(Material.class).stream().map(ItemStack::new).collect(Collectors.toSet()), Range.ANY, Range.ANY, Set.of(Enchantment.ANY), Set.of(Enchantment.ANY));

        private final Set<ItemStack> includes;
        private final Range count;
        private final Range durability;
        private final Set<Enchantment> enchantments;
        private final Set<Enchantment> storedEnchantments;

        private Item(Set<ItemStack> includes, Range count, Range durability, Set<Enchantment> enchantments, Set<Enchantment> storedEnchantments) {
            this.includes = includes;
            this.count = count;
            this.durability = durability;
            this.enchantments = enchantments;
            this.storedEnchantments = storedEnchantments;
        }

        /**
         * Fetches an immutable copy of the items that are included in this predicate that the item can match.
         * @return Included Items
         */
        @NotNull
        public Set<ItemStack> getIncludes() {
            return ImmutableSet.copyOf(includes);
        }

        /**
         * Fetches the range for the amount for this predicate.
         * @return Count Range
         */
        @NotNull
        public Range getCountRange() {
            return count;
        }

        /**
         * Fetches the range for the durability for this predicate.
         * @return Durability Range
         */
        public Range getDurabilityRange() {
            return durability;
        }

        /**
         * Fetches an immutable copy of the enchantments that are required for this predicate.
         * @return Enchantments
         */
        public Set<Enchantment> getEnchantments() {
            return ImmutableSet.copyOf(enchantments);
        }

        /**
         * Fetches an immutable copy of the stored enchantments (For Enchanting Books) that are required for this predicate.
         * @return Stored Enchantments
         */
        public Set<Enchantment> getStoredEnchantments() {
            return ImmutableSet.copyOf(storedEnchantments);
        }

        /**
         * Creates a new Item Predicate Builder.
         * @return Item Predicate Builder
         */
        @NotNull
        public static Builder builder() {
            return new Builder();
        }

        /**
         * Represents an Item Predicate Builder.
         */
        public static final class Builder {
            private Builder() {}

            private final Set<ItemStack> includes = new HashSet<>();
            private Range count = Range.ANY;
            private Range durability = Range.ANY;
            private Set<Enchantment> enchantments = new HashSet<>();
            private Set<Enchantment> storedEnchantments = new HashSet<>();

            /**
             * Adds an item to the included items for this item predicate.
             * @param item Item
             * @return this builder, for chaining
             */
            @NotNull
            public Builder include(@NotNull ItemStack item) {
                includes.add(item);
                return this;
            }

            /**
             * Adds an array of items to the included items for this item predicate.
             * @param items Array of Items
             * @return this builder, for chaining
             */
            public Builder include(@NotNull ItemStack... items) {
                includes.addAll(Arrays.asList(items));
                return this;
            }

            /**
             * Adds an iterable of items to the included items for this item predicate.
             * @param items Iterable of Items
             * @return this builder, for chaining
             */
            public Builder include(@NotNull Iterable<? extends ItemStack> items) {
                items.forEach(includes::add);
                return this;
            }

            /**
             * Sets the count range for this item predicate.
             * @param count Count Range
             * @return this builder, for chaining
             */
            @NotNull
            public Builder count(@NotNull Range count) {
                this.count = count;
                return this;
            }

            /**
             * Sets the count range for this item predicate.
             * @param count Count
             * @return this builder, for chaining
             */
            @NotNull
            public Builder count(int count) {
                return count(Range.exact(count));
            }

            /**
             * Sets the durability range for this item predicate.
             * @param durability Durability Range
             * @return this builder, for chaining
             */
            @NotNull
            public Builder durability(@NotNull Range durability) {
                this.durability = durability;
                return this;
            }

            /***
             * Sets the durability range for this item predicate.
             * @param durability Durability
             * @return this builder, for chaining
             */
            @NotNull
            public Builder durability(int durability) {
                return durability(Range.exact(durability));
            }

            /**
             * Adds an enchantment to the enchantments for this item predicate.
             * @param enchantmentPredicate Enchantment Predicate
             * @return this builder, for chaining
             */
            @NotNull
            public Builder enchantment(@NotNull Enchantment enchantmentPredicate) {
                enchantments.add(enchantmentPredicate);
                return this;
            }

            /**
             * Adds an enchantment to the enchantments for this item predicate.
             * @param enchantment Enchantment
             * @param level Level Range
             * @return this builder, for chaining
             */
            @NotNull
            public Builder enchantment(@NotNull org.bukkit.enchantments.Enchantment enchantment, @NotNull Range level) {
                enchantments.add(new Enchantment(enchantment, level));
                return this;
            }

            /**
             * Adds an enchantment to the enchantments for this item predicate.
             * @param enchantment Enchantment
             * @param level Level
             * @return this builder, for chaining
             */
            @NotNull
            public Builder enchantment(@NotNull org.bukkit.enchantments.Enchantment enchantment, int level) {
                return enchantment(enchantment, Range.exact(level));
            }

            /**
             * Adds an enchantment to the stored enchantments for this item predicate.
             * @param enchantmentPredicate Enchantment Predicate
             * @return this builder, for chaining
             */
            @NotNull
            public Builder storedEnchantment(@NotNull Enchantment enchantmentPredicate) {
                storedEnchantments.add(enchantmentPredicate);
                return this;
            }

            /**
             * Adds an enchantment to the stored enchantments for this item predicate.
             * @param enchantment Enchantment
             * @param level Level Range
             * @return this builder, for chaining
             */
            @NotNull
            public Builder storedEnchantment(@NotNull org.bukkit.enchantments.Enchantment enchantment, @NotNull Range level) {
                storedEnchantments.add(new Enchantment(enchantment, level));
                return this;
            }

            /**
             * Adds an enchantment to the stored enchantments for this item predicate.
             * @param enchantment Enchantment
             * @param level Level
             * @return this builder, for chaining
             */
            @NotNull
            public Builder storedEnchantment(@NotNull org.bukkit.enchantments.Enchantment enchantment, int level) {
                return storedEnchantment(enchantment, Range.exact(level));
            }

            /**
             * Builds the Item Predicate.
             * @return Item Predicate
             */
            @NotNull
            public Item build() {
                return new Item(includes, count, durability, enchantments, storedEnchantments);
            }

        }

    }

    /**
     * Represents an Enchantment Predicate.
     */
    public static final class Enchantment {

        /**
         * Represents an Enchantment Predicate that matches any enchantment.
         */
        public static final Enchantment ANY = new Enchantment(null, Range.ANY);

        private final org.bukkit.enchantments.Enchantment enchantment;
        private final Range level;

        private Enchantment(org.bukkit.enchantments.Enchantment enchantment, Range level) {
            this.enchantment = enchantment;
            this.level = level;
        }

        /**
         * Fetches the enchantment for this enchantment predicate.
         * @return Bukkit Enchantment
         */
        @NotNull
        public org.bukkit.enchantments.Enchantment getEnchantment() {
            return enchantment;
        }

        /**
         * Fetches the level range for this enchantment predicate.
         * @return Level Range
         */
        @NotNull
        public Range getLevelRange() {
            return level;
        }

        /**
         * Creates a new Enchantment Predicate Builder.
         * @return Enchantment Predicate Builder
         */
        @NotNull
        public static Builder builder() {
            return new Builder();
        }

        /**
         * Represents an Enchantment Predicate Builder.
         */
        public static final class Builder {
            private Builder() {}

            private org.bukkit.enchantments.Enchantment enchantment;
            private Range level = Range.ANY;

            /**
             * Sets the enchantment for this enchantment predicate.
             * @param enchantment Bukkit Enchantment
             * @return this builder, for chaining
             */
            public Builder enchantment(@NotNull org.bukkit.enchantments.Enchantment enchantment) {
                this.enchantment = enchantment;
                return this;
            }

            /**
             * Sets the level range for this enchantment predicate.
             * @param level Level Range
             * @return this builder, for chaining
             */
            public Builder level(@NotNull Range level) {
                this.level = level;
                return this;
            }

            /**
             * Sets the level range for this enchantment predicate.
             * @param level Level
             * @return this builder, for chaining
             */
            public Builder level(int level) {
                this.level = new Range(level, level);
                return this;
            }

            public ATriggerPredicate.Enchantment build() {
                if (level == null) throw new IllegalStateException("Level Range cannot be null");
                if (enchantment == null) throw new IllegalStateException("Enchantment cannot be null");

                return new ATriggerPredicate.Enchantment(enchantment, level);
            }
        }

    }

    /**
     * Represents a Light Predicate for recording light levels.
     */
    public static final class Light {

        /**
         * Represents a light predicate that matches any light level.
         */
        public static final Light ANY = new Light(Range.ANY);

        private final Range brightness;

        private Light(Range brightness) {
            this.brightness = brightness;
        }

        /**
         * Fetches the brightness range for this light predicate.
         * @return Brightness Range
         */
        @NotNull
        public Range getBrightnessRange() {
            return brightness;
        }

        public static Builder builder() {
            return new Builder();
        }

        public static final class Builder {

            private Range brightness = Range.ANY;

            /**
             * Sets the brightness range for this light predicate.
             * @param brightness Brightness Range
             * @return this builder, for chaining
             */
            public Builder brightness(@NotNull Range brightness) {
                this.brightness = brightness;
                return this;
            }

            /**
             * Sets the brightness range for this light predicate.
             * @param brightness Brightness
             * @return this builder, for chaining
             */
            public Builder brightness(int brightness) {
                return brightness(Range.exact(brightness));
            }

            /**
             * Builds the light predicate.
             * @return Light Predicate
             */
            @NotNull
            public Light build() {
                return new Light(brightness);
            }

        }

    }

    /**
     * Represents a Block Predicate.
     */
    public static final class Block {

        /**
         * Represents a block predicate that matches any block.
         */
        public static final Block ANY = new Block(EnumSet.allOf(Material.class));

        private final Set<Material> includes;

        private Block(Set<Material> includes) {
            this.includes = includes;
        }

        /**
         * Fetches an immutable set of the materials for this block predicate.
         * @return Materials
         */
        @NotNull
        public Set<Material> getMaterials() {
            return ImmutableSet.copyOf(includes);
        }

        /**
         * Creates a new Block Predicate Builder.
         * @return Block Predicate Builder
         */
        @NotNull
        public static Builder builder() {
            return new Builder();
        }

        /**
         * Represents a Block Predicate Builder.
         */
        public static final class Builder {

            private Builder() {
            }

            private final Set<Material> includes = new HashSet<>();

            /**
             * Adds a material to the materials for this block predicate.
             * @param material Material
             * @return this builder, for chaining
             */
            @NotNull
            public Builder material(@NotNull Material material) {
                includes.add(material);
                return this;
            }

            /**
             * Adds an array of materials to the materials for this block predicate.
             * @param materials Array of Materials
             * @return this builder, for chaining
             */
            @NotNull
            public Builder material(@NotNull Material... materials) {
                includes.addAll(Arrays.asList(materials));
                return this;
            }

            /**
             * Adds an iterable of material to the materials for this block predicate.
             * @param materials Iterable of Materials
             * @return this builder, for chaining
             */
            @NotNull
            public Builder material(@NotNull Iterable<? extends Material> materials) {
                materials.forEach(includes::add);
                return this;
            }

            /**
             * Builds the Block Predicate.
             * @return Block Predicate
             */
            @NotNull
            public Block build() {
                return new Block(includes);
            }
        }

    }

    /**
     * Represents a Location Predicate.
     */
    public static final class Location {

        /**
         * Represents a Location Predicate that matches any location.
         */
        public static final Location ANY = new Location(Range.ANY, Range.ANY, Range.ANY, null, null, false, null, null);

        private final Range xRange;
        private final Range yRange;
        private final Range zRange;
        private final Biome biome;
        private final World dimension;
        private final boolean smokey;
        private final Light lightPredicate;
        private final Block blockPredicate;

        private Location(Range xRange, Range yRange, Range zRange, Biome biome, World dimension, boolean smokey, Light lightPredicate, Block blockPredicate) {
            this.xRange = xRange;
            this.yRange = yRange;
            this.zRange = zRange;
            this.biome = biome;
            this.dimension = dimension;
            this.smokey = smokey;
            this.lightPredicate = lightPredicate;
            this.blockPredicate = blockPredicate;
        }

        /**
         * Fetches the X range for this location predicate.
         * @return X Range
         */
        @NotNull
        public Range getXRange() {
            return xRange;
        }

        /**
         * Fetches the Y range for this location predicate.
         * @return Y Range
         */
        @NotNull
        public Range getYRange() {
            return yRange;
        }

        /**
         * Fetches the Z range for this location predicate.
         * @return Z Range
         */
        @NotNull
        public Range getZRange() {
            return zRange;
        }

        /**
         * Fetches the biome for this location predicate.
         * @return Biome
         */
        @Nullable
        public Biome getBiome() {
            return biome;
        }

        /**
         * Fetches the dimension for this location predicate.
         * @return Dimension
         */
        @Nullable
        public World getDimension() {
            return dimension;
        }

        /**
         * Fetches whether this location predicate allows smokey locations from campfires.
         * @return true if smokey, false otherwise
         */
        public boolean isSmokey() {
            return smokey;
        }

        /**
         * Fetches the light predicate for this location predicate.
         * @return Light Predicate
         */
        @Nullable
        public Light getLightPredicate() {
            return lightPredicate;
        }

        /**
         * Fetches the block predicate for this location predicate.
         * @return Block Predicate
         */
        @Nullable
        public Block getBlockPredicate() {
            return blockPredicate;
        }

        /**
         * Creates a new Location Predicate Builder.
         * @return Location Predicate Builder
         */
        @NotNull
        public static Builder builder() {
            return new Builder();
        }

        /**
         * Represents a Location Predicate Builder.
         */
        public static final class Builder {

            private Builder() {}

            private Range xRange = Range.ANY;
            private Range yRange = Range.ANY;
            private Range zRange = Range.ANY;
            private Biome biome = null;
            private World dimension = null;
            private boolean smokey = false;
            private Light lightPredicate = null;
            private Block blockPredicate = null;

            /**
             * Sets the X range for this location predicate.
             * @param xRange X Range
             * @return this builder, for chaining
             */
            @NotNull
            public Builder x(@NotNull Range xRange) {
                this.xRange = xRange;
                return this;
            }

            /**
             * Sets the X range for this location predicate.
             * @param x X
             * @return this builder, for chaining
             */
            @NotNull
            public Builder x(int x) {
                return x(Range.exact(x));
            }

            /**
             * Sets the Y range for this location predicate.
             * @param yRange Y Range
             * @return this builder, for chaining
             */
            @NotNull
            public Builder y(@NotNull Range yRange) {
                this.yRange = yRange;
                return this;
            }

            /**
             * Sets the Y range for this location predicate.
             * @param y Y
             * @return this builder, for chaining
             */
            @NotNull
            public Builder y(int y) {
                return y(Range.exact(y));
            }

            /**
             * Sets the Z range for this location predicate.
             * @param zRange Z Range
             * @return this builder, for chaining
             */
            @NotNull
            public Builder z(@NotNull Range zRange) {
                this.zRange = zRange;
                return this;
            }

            /**
             * Sets the Z range for this location predicate.
             * @param z Z
             * @return this builder, for chaining
             */
            @NotNull
            public Builder z(int z) {
                return z(Range.exact(z));
            }

            /**
             * Sets the biome for this location predicate.
             * @param biome Biome
             * @return this builder, for chaining
             */
            @NotNull
            public Builder biome(@Nullable Biome biome) {
                this.biome = biome;
                return this;
            }

            /**
             * Sets the dimension for this location predicate.
             * @param dimension Dimension
             * @return this builder, for chaining
             */
            @NotNull
            public Builder dimension(@Nullable World dimension) {
                this.dimension = dimension;
                return this;
            }

            /**
             * Sets whether this location predicate allows smokey locations from campfires.
             * @param smokey true if smokey, false otherwise
             * @return this builder, for chaining
             */
            @NotNull
            public Builder smokey(boolean smokey) {
                this.smokey = smokey;
                return this;
            }

            /**
             * Sets the light predicate for this location predicate.
             * @param lightPredicate Light Predicate
             * @return this builder, for chaining
             */
            @NotNull
            public Builder light(@Nullable Light lightPredicate) {
                this.lightPredicate = lightPredicate;
                return this;
            }

            /**
             * Sets the block predicate for this location predicate.
             * @param blockPredicate Block Predicate
             * @return this builder, for chaining
             */
            @NotNull
            public Builder block(@Nullable Block blockPredicate) {
                this.blockPredicate = blockPredicate;
                return this;
            }

            /**
             * Builds the Location Predicate.
             * @return Location Predicate
             */
            @NotNull
            public Location build() {
                return new Location(xRange, yRange, zRange, biome, dimension, smokey, lightPredicate, blockPredicate);
            }

        }

    }

    /**
     * Represents an Entity Predicate.
     */
    public static final class Entity {

        /**
         * Represents an Entity Predicate that matches any entity.
         */
        public static final Entity ANY = new Entity(
                EnumSet.allOf(EntityType.class),
                Range.ANY,
                Location.ANY,
                Location.ANY,
                false,
                false,
                false,
                false,
                false,
                Collections.emptyMap(),
                null,
                null,
                null
        );

        private final Set<EntityType> types;
        private final Range distanceToPlayer;
        private final Location location;
        private final Location steppingLocation;

        // Entity Flags
        private final boolean isOnFire;
        private final boolean isCrouching;
        private final boolean isSprinting;
        private final boolean isSwimming;
        private final boolean isBaby;

        // Entity Equipment
        private final Map<EquipmentSlot, Item> equipment;

        private final Entity vehicle;
        private final Entity passenger;
        private final Entity target;

        private Entity(Set<EntityType> types, Range distanceToPlayer, Location location, Location steppingLocation, boolean isOnFire, boolean isCrouching, boolean isSprinting, boolean isSwimming,
                       boolean isBaby, Map<EquipmentSlot, Item> equipment, Entity vehicle, Entity passenger, Entity target) {

            this.types = types;
            this.distanceToPlayer = distanceToPlayer;
            this.location = location;
            this.steppingLocation = steppingLocation;
            this.isOnFire = isOnFire;
            this.isCrouching = isCrouching;
            this.isSprinting = isSprinting;
            this.isSwimming = isSwimming;
            this.isBaby = isBaby;
            this.equipment = equipment;
            this.vehicle = vehicle;
            this.passenger = passenger;
            this.target = target;
        }

        /**
         * Fetches an immutable set of the types of entities that this predicate will match.
         * @return Entity Types
         */
        @NotNull
        public Set<EntityType> getTypes() {
            return ImmutableSet.copyOf(types);
        }

        /**
         * Fetches the range of the distance to the player that this predicate will match.
         * @return Distance to Player Range
         */
        @NotNull
        public Range getPlayerDistanceRange() {
            return distanceToPlayer;
        }

        /**
         * Fetches the location predicate of the entity that this predicate will match.
         * @return Location Predicate
         */
        @NotNull
        public Location getLocation() {
            return location;
        }

        /**
         * Fetches the location predicate of the block that the entity is stepping on that this predicate will match.
         * @return Stepping Location Predicate
         */
        @NotNull
        public Location getSteppingLocation() {
            return steppingLocation;
        }

        /**
         * Returns whether this entity predicate will match entities that are on fire.
         * @return true if on fire, false otherwise
         */
        public boolean isOnFire() {
            return isOnFire;
        }

        /**
         * Returns whether this entity predicate will match entities that are crouching.
         * @return true if crouching, false otherwise
         */
        public boolean isCrouching() {
            return isCrouching;
        }

        /**
         * Returns whether this entity predicate will match entities that are sprinting.
         * @return true if sprinting, false otherwise
         */
        public boolean isSprinting() {
            return isSprinting;
        }

        /**
         * Returns whether this entity predicate will match entities that are swimming.
         * @return true if swimming, false otherwise
         */
        public boolean isSwimming() {
            return isSwimming;
        }

        /**
         * Returns whether this entity predicate will match entities that are babies.
         * @return true if baby, false otherwise
         */
        public boolean isBaby() {
            return isBaby;
        }

        /**
         * Fetches an immutable map of the equipment that this predicate will match.
         * @return Equipment Map
         */
        @NotNull
        public Map<EquipmentSlot, Item> getEquipment() {
            return ImmutableMap.copyOf(equipment);
        }

        /**
         * Fetches the entity predicate for the vehicle this entity may reside in, that this predicate will match.
         * @return Vehicle Entity Predicate
         */
        @NotNull
        public Entity getVehicle() {
            return vehicle;
        }

        /**
         * Fetches the entity predicate for the passenger this entity may have, that this predicate will match.
         * @return Passenger Entity Predicate
         */
        @NotNull
        public Entity getPassenger() {
            return passenger;
        }

        /**
         * Fetches the entity predicate for the target this entity may have, that this predicate will match.
         * @return Target Entity Predicate
         */
        @NotNull
        public Entity getTarget() {
            return target;
        }

        /**
         * Creates a new Entity Predicate Builder.
         * @return Entity Predicate Builder
         */
        @NotNull
        public static Builder builder() {
            return new Builder();
        }

        /**
         * Represents an Entity Predicate Builder.
         */
        public static final class Builder {

            private Builder() {}

            private final Set<EntityType> types = new HashSet<>();
            private Range distanceToPlayer = Range.ANY;
            private Location location = Location.ANY;
            private Location steppingLocation = Location.ANY;

            private boolean isOnFire = false;
            private boolean isCrouching = false;
            private boolean isSprinting = false;
            private boolean isSwimming = false;
            private boolean isBaby = false;

            private final Map<EquipmentSlot, Item> equipment = new HashMap<>();

            private Entity vehicle = Entity.ANY;
            private Entity passenger = Entity.ANY;
            private Entity target = Entity.ANY;

            /**
             * Adds an entity type to this entity predicate builder.
             * @param type Entity Type
             * @return this builder, for chaining
             */
            @NotNull
            public Builder type(@NotNull EntityType type) {
                this.types.add(type);
                return this;
            }

            /**
             * Adds an array of entity types to this entity predicate builder.
             * @param types Array of Entity Types
             * @return this builder, for chaining
             */
            @NotNull
            public Builder types(@NotNull EntityType... types) {
                this.types.addAll(Arrays.asList(types));
                return this;
            }

            /**
             * Adds an iterable of entity types to this entity predicate builder.
             * @param types Collection of Entity Types
             * @return this builder, for chaining
             */
            @NotNull
            public Builder types(@NotNull Iterable<? extends EntityType> types) {
                types.forEach(this.types::add);
                return this;
            }

            /**
             * Sets the distance to player range of this entity predicate builder.
             * @param distanceToPlayer Distance to Player Range
             * @return this builder, for chaining
             */
            @NotNull
            public Builder distanceToPlayer(@NotNull Range distanceToPlayer) {
                this.distanceToPlayer = distanceToPlayer;
                return this;
            }

            /**
             * Sets the location predicate of this entity predicate builder.
             * @param location Location Predicate
             * @return this builder, for chaining
             */
            @NotNull
            public Builder location(@NotNull Location location) {
                this.location = location;
                return this;
            }

            /**
             * Sets the stepping location predicate of this entity predicate builder.
             * @param steppingLocation Stepping Location Predicate
             * @return this builder, for chaining
             */
            @NotNull
            public Builder steppingLocation(@NotNull Location steppingLocation) {
                this.steppingLocation = steppingLocation;
                return this;
            }

            /**
             * Sets whether this entity predicate builder will match entities that are on fire.
             * @param isOnFire true if on fire, false otherwise
             * @return this builder, for chaining
             */
            @NotNull
            public Builder onFire(boolean isOnFire) {
                this.isOnFire = isOnFire;
                return this;
            }

            /**
             * Sets whether this entity predicate builder will match entities that are crouching.
             * @param isCrouching true if crouching, false otherwise
             * @return this builder, for chaining
             */
            @NotNull
            public Builder crouching(boolean isCrouching) {
                this.isCrouching = isCrouching;
                return this;
            }

            /**
             * Sets whether this entity predicate builder will match entities that are sprinting.
             * @param isSprinting true if sprinting, false otherwise
             * @return this builder, for chaining
             */
            @NotNull
            public Builder sprinting(boolean isSprinting) {
                this.isSprinting = isSprinting;
                return this;
            }

            /**
             * Sets whether this entity predicate builder will match entities that are swimming.
             * @param isSwimming true if swimming, false otherwise
             * @return this builder, for chaining
             */
            @NotNull
            public Builder swimming(boolean isSwimming) {
                this.isSwimming = isSwimming;
                return this;
            }

            /**
             * Sets whether this entity predicate builder will match entities that are babies.
             * @param isBaby true if baby, false otherwise
             * @return this builder, for chaining
             */
            @NotNull
            public Builder baby(boolean isBaby) {
                this.isBaby = isBaby;
                return this;
            }

            /**
             * Adds an equipment slot and item to this entity predicate builder.
             * @param slot Equipment Slot
             * @param item Item
             * @return this builder, for chaining
             */
            @NotNull
            public Builder equipment(@NotNull EquipmentSlot slot, @NotNull Item item) {
                this.equipment.put(slot, item);
                return this;
            }

            /**
             * Adds an equipment map to this entity predicate builder.
             * @param equipment Equipment Map
             * @return this builder, for chaining
             */
            @NotNull
            public Builder equipment(@NotNull Map<EquipmentSlot, Item> equipment) {
                this.equipment.putAll(equipment);
                return this;
            }

            /**
             * Sets the vehicle entity predicate of this entity predicate builder.
             * @param vehicle Vehicle Entity Predicate
             * @return this builder, for chaining
             */
            @NotNull
            public Builder vehicle(@NotNull Entity vehicle) {
                this.vehicle = vehicle;
                return this;
            }

            /**
             * Sets the passenger entity predicate of this entity predicate builder.
             * @param passenger Passenger Entity Predicate
             * @return this builder, for chaining
             */
            @NotNull
            public Builder passenger(@NotNull Entity passenger) {
                this.passenger = passenger;
                return this;
            }

            /**
             * Sets the target entity predicate of this entity predicate builder.
             * @param target Target Entity Predicate
             * @return this builder, for chaining
             */
            @NotNull
            public Builder target(@NotNull Entity target) {
                this.target = target;
                return this;
            }

            /**
             * Builds the entity predicate from this entity predicate builder.
             * @return Entity Predicate
             */
            @NotNull
            public Entity build() {
                return new Entity(
                        this.types,
                        this.distanceToPlayer,
                        this.location,
                        this.steppingLocation,
                        this.isOnFire,
                        this.isCrouching,
                        this.isSprinting,
                        this.isSwimming,
                        this.isBaby,
                        this.equipment,
                        this.vehicle,
                        this.passenger,
                        this.target
                );
            }
        }
    }
}
