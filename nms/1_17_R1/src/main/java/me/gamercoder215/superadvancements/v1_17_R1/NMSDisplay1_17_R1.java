package me.gamercoder215.superadvancements.v1_17_R1;

import me.gamercoder215.superadvancements.advancement.ADisplay;
import me.gamercoder215.superadvancements.advancement.AFrame;
import net.minecraft.advancements.AdvancementDisplay;

import java.util.Arrays;

final class NMSDisplay1_17_R1 extends ADisplay {

    private final AdvancementDisplay handle;

    NMSDisplay1_17_R1(AdvancementDisplay handle) {
        this.handle = handle;

        this.setFrame(Arrays.stream(AFrame.values()).filter(f -> f.name().equalsIgnoreCase(handle.e().a())).findFirst().orElse(AFrame.TASK));
        this.setIcon(Wrapper1_17_R1.fromNMS(handle.c()));
        this.setX(handle.f());
        this.setY(handle.g());

        if (handle.d() != null) this.setBackgroundTexture(handle.d().toString());
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
