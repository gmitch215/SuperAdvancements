package me.gamercoder215.superadvancements.v1_19_R3;

import me.gamercoder215.superadvancements.wrapper.Wrapper;
import net.minecraft.world.level.block.state.properties.Property;
import org.bukkit.block.BlockState;
import org.bukkit.craftbukkit.v1_19_R3.block.CraftBlockState;

import java.util.HashSet;
import java.util.Set;

@SuppressWarnings({"unchecked", "rawtypes"})
public final class Wrapper1_19_R3 implements Wrapper {

    private static Set<Property.Value<?>> toNMS(BlockState state) {
        net.minecraft.world.level.block.state.BlockState nms = ((CraftBlockState) state).getHandle();
        Set<Property.Value<?>> set = new HashSet<>();

        for (Property<? extends Comparable<?>> p : nms.getBlock().getStateDefinition().getProperties())
            set.add(new Property.Value(p, nms.getValue(p)));

        return set;
    }

}
