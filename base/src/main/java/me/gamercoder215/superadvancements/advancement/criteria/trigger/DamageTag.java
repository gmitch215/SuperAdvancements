package me.gamercoder215.superadvancements.advancement.criteria.trigger;

import org.bukkit.Keyed;
import org.bukkit.NamespacedKey;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.entity.EntityType;
import org.bukkit.event.entity.EntityDamageEvent;
import org.bukkit.potion.PotionEffectType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Represents a Damage Tag for the Damage Criteria Trigger
 */
public enum DamageTag implements Keyed {

    /**
     * Damage that damages the player's helmet
     */
    DAMAGES_HELMET("damages_helmet"),

    /**
     * Damage that bypasses armor reduction
     */
    BYPASSES_ARMOR("bypasses_armor"),

    /**
     * Damage that bypasses shield blocks
     */
    BYPASSES_SHIELD("bypasses_shield"),

    /**
     * Damage that bypasses invulnerability ticks
     */
    BYPASSES_INVULNERABILITY("bypasses_invulnerability"),

    /**
     * Damage that bypasses the cooldown for attacking
     */
    BYPASSES_COOLDOWN("bypasses_cooldown"),

    /**
     * Damage that bypasses potion effects
     */
    BYPASSES_EFFECTS("bypasses_effects"),

    /**
     * Damage that bypasses {@link PotionEffectType#DAMAGE_RESISTANCE}
     */
    BYPASSES_RESISTANCE("bypasses_resistance"),

    /**
     * Damage that bypasses enchantment protection on armor, such as {@link Enchantment#PROTECTION_ENVIRONMENTAL}
     */
    BYPASSES_ENCHANTMENTS("bypasses_enchantments"),

    /**
     * Damage that is from fire or lavadamage
     */
    IS_FIRE("is_fire"),

    /**
     * Damage that is from a projectile
     */
    IS_PROJECTILE("is_projectile"),

    /**
     * Damage that {@link EntityType#WITCH} is immune to
     */
    WITCH_RESISTANT_TO("witch_resistant_to"),

    /**
     * Damage that is from an explosion
     */
    IS_EXPLOSION("is_explosion"),

    /**
     * Damage that is from fall damage
     */
    IS_FALL("is_fall"),

    /**
     * Damage that is from drowning
     */
    IS_DROWNING("is_drowning"),

    /**
     * Damage that is from freezing in powdered snow
     */
    IS_FREEZING("is_freezing"),

    /**
     * Damage that is from a lightning strike
     */
    IS_LIGHTNING("is_lightning"),

    /**
     * Damage that is not from a neutral entity being angry
     */
    NO_ANGER("no_anger"),

    /**
     * Damage that does not actually reduce health points
     */
    NO_IMPACT("no_impact"),

    /**
     * Damage that is always the most significant fall damage
     */
    ALWAYS_MOST_SIGNIFICANT_FALL("always_most_significant_fall"),

    /**
     * Damage that {@link EntityType#WITHER} is immune to
     */
    WITHER_IMMUNE_TO("wither_immune_to"),

    /**
     * Damage that causes armor stands to catch on fire
     */
    IGNITES_ARMOR_STANDS("ignites_armor_stands"),

    /**
     * Damage that causes armor stands to break from burning
     */
    BURNS_ARMOR_STANDS("burns_armor_stands"),

    /**
     * Damage that avoids guardian thorn attacks
     */
    AVOIDS_GUARDIAN_THORNS("avoids_guardian_thorns"),

    /**
     * Damage that triggers spawning silverfish from infested blocks
     */
    ALWAYS_TRIGGERS_SILVERFISH("always_triggers_silverfish"),

    /**
     * Damage that hurts {@link EntityType#ENDER_DRAGON}
     */
    ALWAYS_HURTS_ENDER_DRAGONS("always_hurts_ender_dragons"),

    ;

    private final String key;

    DamageTag(String key) {
        this.key = key;
    }

    @Override
    public NamespacedKey getKey() {
        return NamespacedKey.minecraft(key);
    }

    /**
     * Matches a DamageCause to a DamageTag.
     * @param cause DamageCause to match
     * @return DamageTag that matches the DamageCause, or null if none match
     */
    @Nullable
    public static DamageTag from(@NotNull EntityDamageEvent.DamageCause cause) {
        switch (cause.name()) {
            case "FALL":
                return IS_FALL;
            case "FIRE_TICK":
            case "LAVA":
            case "FIRE":
            case "HOT_FLOOR":
                return IS_FIRE;
            case "DROWNING":
                return IS_DROWNING;
            case "FREEZE":
                return IS_FREEZING;
           case "LIGHTNING":
                return IS_LIGHTNING;
            case "PROJECTILE":
                return IS_PROJECTILE;
            case "BLOCK_EXPLOSION":
            case "EXPLOSION":
                return IS_EXPLOSION;
        }

        return null;
    }
}
