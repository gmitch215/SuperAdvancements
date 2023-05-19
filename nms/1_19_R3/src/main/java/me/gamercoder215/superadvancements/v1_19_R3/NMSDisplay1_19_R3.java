package me.gamercoder215.superadvancements.v1_19_R3;

import me.gamercoder215.superadvancements.advancement.ADisplay;
import me.gamercoder215.superadvancements.advancement.AFrame;
import net.minecraft.advancements.DisplayInfo;

import java.util.Arrays;

final class NMSDisplay1_19_R3 extends ADisplay {

    private final DisplayInfo handle;

    NMSDisplay1_19_R3(DisplayInfo handle) {
        this.handle = handle;

        this.setFrame(Arrays.stream(AFrame.values()).filter(f -> f.name().equalsIgnoreCase(handle.getFrame().getName())).findFirst().orElse(AFrame.TASK));
        this.setIcon(Wrapper1_19_R3.fromNMS(handle.getIcon()));
        this.setBackgroundTexture(handle.getBackground().toString());
        this.setX(handle.getX());
        this.setY(handle.getY());
    }


    @Override
    public String getTitleAsString() {
        return handle.getTitle().getString();
    }

    @Override
    public String getDescriptionAsString() {
        return handle.getDescription().getString();
    }
}
