package me.gamercoder215.superadvancements.v1_16_R3;

import me.gamercoder215.superadvancements.advancement.ADisplay;
import me.gamercoder215.superadvancements.advancement.AFrame;
import net.minecraft.server.v1_16_R3.AdvancementDisplay;
import net.minecraft.server.v1_16_R3.ItemStack;
import net.minecraft.server.v1_16_R3.MinecraftKey;

import java.util.Arrays;

import static me.gamercoder215.superadvancements.v1_16_R3.Wrapper1_16_R3.getFloat;
import static me.gamercoder215.superadvancements.v1_16_R3.Wrapper1_16_R3.getObject;

final class NMSDisplay1_16_R3 extends ADisplay {

    private final AdvancementDisplay handle;

    NMSDisplay1_16_R3(AdvancementDisplay handle) {
        this.handle = handle;

        this.setFrame(Arrays.stream(AFrame.values()).filter(f -> f.name().equalsIgnoreCase(handle.e().a())).findFirst().orElse(AFrame.TASK));
        this.setIcon(Wrapper1_16_R3.fromNMS(getObject(handle, "c", ItemStack.class)));
        this.setX(getFloat(handle, "i"));
        this.setY(getFloat(handle, "j"));

        MinecraftKey bg = getObject(handle, "d", MinecraftKey.class);
        if (bg != null) this.setBackgroundTexture(bg.toString());
    }


    @Override
    public String getTitleAsString() {
        return handle.a().getString();
    }

    @Override
    public String getDescriptionAsString() {
        return handle.b().getString();
    }
}
