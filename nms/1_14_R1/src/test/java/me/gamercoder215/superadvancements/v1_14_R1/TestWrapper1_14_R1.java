package me.gamercoder215.superadvancements.v1_14_R1;

import me.gamercoder215.superadvancements.advancement.AReward;
import net.minecraft.server.v1_14_R1.MinecraftKey;
import net.minecraft.server.v1_14_R1.SharedConstants;
import org.bukkit.NamespacedKey;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.Set;

@SuppressWarnings("unused")
public class TestWrapper1_14_R1 {

    @BeforeAll
    public static void setup() {
        try {
            Field gameVersionF = SharedConstants.class.getDeclaredField("d");
            gameVersionF.setAccessible(true);
            gameVersionF.set(null, new MockWorldVersion1_14_R1());
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }

        SharedConstants.b = true;
    }

    private final boolean b = true;
    private final String s = "test";
    private final int i = 3;

    // Advancement Objects

    private final AReward reward = new AReward(20, Set.of(NamespacedKey.minecraft("dummy")));

    @Test
    @DisplayName("Test Wrapper1_14_R1#getObject")
    public void testReflection() {
        Assertions.assertTrue(Wrapper1_14_R1.getBoolean(this, "b"));
        Assertions.assertEquals(Wrapper1_14_R1.getObject(this, "s", String.class), "test");
        Assertions.assertEquals(Wrapper1_14_R1.getInt(this, "i"), 3);
    }

    @Test
    @DisplayName("Test Wrapper1_14_R1#toNMS")
    public void testToNMS() {
        Assertions.assertNotNull(Wrapper1_14_R1.toNMS(NamespacedKey.minecraft("eee")));
        Assertions.assertNotNull(Wrapper1_14_R1.toNMS(reward));
    }

    @Test
    @DisplayName("Test Wrapper1_14_R1#fromNMS")
    public void testFromNMS() {
        Assertions.assertNotNull(Wrapper1_14_R1.fromNMS(new MinecraftKey("eee")));
    }

}