package me.gamercoder215.superadvancements.v1_13_R2;

import me.gamercoder215.superadvancements.advancement.AReward;
import net.minecraft.server.v1_13_R2.MinecraftKey;
import net.minecraft.server.v1_13_R2.SharedConstants;
import org.bukkit.NamespacedKey;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.Set;

@SuppressWarnings("unused")
public class TestWrapper1_13_R2 {

    @BeforeAll
    public static void setup() {
        SharedConstants.b = true;
    }

    private final boolean b = true;
    private final String s = "test";
    private final int i = 3;

    // Advancement Objects

    private final AReward reward = new AReward(20, Set.of(NamespacedKey.minecraft("dummy")));

    @Test
    @DisplayName("Test Wrapper1_13_R2#getObject")
    public void testReflection() {
        Assertions.assertTrue(Wrapper1_13_R2.getBoolean(this, "b"));
        Assertions.assertEquals(Wrapper1_13_R2.getObject(this, "s", String.class), "test");
        Assertions.assertEquals(Wrapper1_13_R2.getInt(this, "i"), 3);
    }

    @Test
    @DisplayName("Test Wrapper1_13_R2#toNMS")
    public void testToNMS() {
        Assertions.assertNotNull(Wrapper1_13_R2.toNMS(NamespacedKey.minecraft("eee")));
        Assertions.assertNotNull(Wrapper1_13_R2.toNMS(reward));
    }

    @Test
    @DisplayName("Test Wrapper1_13_R2#fromNMS")
    public void testFromNMS() {
        Assertions.assertNotNull(Wrapper1_13_R2.fromNMS(new MinecraftKey("eee")));
    }

}