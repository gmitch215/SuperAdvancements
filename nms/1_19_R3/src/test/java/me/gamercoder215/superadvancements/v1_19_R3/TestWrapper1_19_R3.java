package me.gamercoder215.superadvancements.v1_19_R3;

import java.util.Set;

import org.bukkit.NamespacedKey;
import org.bukkit.block.Biome;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import me.gamercoder215.superadvancements.advancement.AReward;
import me.gamercoder215.superadvancements.advancement.criteria.trigger.ATriggerPredicate;
import me.gamercoder215.superadvancements.advancement.criteria.trigger.DamageTag;
import me.gamercoder215.superadvancements.util.Range;
import net.minecraft.SharedConstants;
import net.minecraft.advancements.AdvancementRewards;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.Bootstrap;

@SuppressWarnings("unused")
public class TestWrapper1_19_R3 {

    @BeforeAll
    public static void setup() {
        SharedConstants.setVersion(new MockWorldVersion1_19_R3());
        SharedConstants.IS_RUNNING_IN_IDE = true;
        Bootstrap.bootStrap();
    }

    private final ATriggerPredicate.Light light = ATriggerPredicate.Light.of(Range.of(12, 15));

    private final ATriggerPredicate.Location loc = ATriggerPredicate.Location.builder()
            .x(1)
            .y(7)
            .z(-14)
            .light(light)
            .biome(Biome.BADLANDS)
            .build();

    private final ATriggerPredicate.Entity entity = ATriggerPredicate.Entity.builder()
            .baby(true)
            .onFire(true)
            .swimming(false)
            .location(loc)
            .steppingLocation(loc)
            .build();

    private final ATriggerPredicate.Damage damage = ATriggerPredicate.Damage.builder()
            .blocked(true)
            .source(entity)
            .cause(DamageTag.IS_PROJECTILE)
            .dealt(Range.exact(1.0))
            .taken(Range.exact(2.0))
            .build();

    private final boolean b = true;
    private final String s = "test";
    private final int i = 3;

    // Advancement Objects

    private final AReward reward = new AReward(20, Set.of(NamespacedKey.minecraft("dummy")));

    @Test
    @DisplayName("Test Wrapper1_19_R3#getObject")
    public void testReflection() {
        Assertions.assertTrue(Wrapper1_19_R3.getBoolean(this, "b"));
        Assertions.assertEquals(Wrapper1_19_R3.getObject(this, "s", String.class), "test");
        Assertions.assertEquals(Wrapper1_19_R3.getInt(this, "i"), 3);
    }

    @Test
    @DisplayName("Test Wrapper1_19_R3#toNMS")
    public void testToNMS() {
        Assertions.assertNotNull(Wrapper1_19_R3.toNMS(NamespacedKey.minecraft("eee")));

        Assertions.assertNotNull(Wrapper1_19_R3.toNMS(light));
        Assertions.assertNotNull(Wrapper1_19_R3.toNMS(loc));
        Assertions.assertNotNull(Wrapper1_19_R3.toNMS(entity));
        Assertions.assertNotNull(Wrapper1_19_R3.toNMS(damage));

        Assertions.assertNotNull(Wrapper1_19_R3.toNMS(reward));
    }

    @Test
    @DisplayName("Test Wrapper1_19_R3#fromNMS")
    public void testFromNMS() {
        Assertions.assertNotNull(Wrapper1_19_R3.fromNMS(new ResourceLocation("eee")));
    }

}