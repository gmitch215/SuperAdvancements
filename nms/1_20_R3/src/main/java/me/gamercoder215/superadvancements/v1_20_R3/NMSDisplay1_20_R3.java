package me.gamercoder215.superadvancements.v1_20_R3;

import me.gamercoder215.superadvancements.advancement.ADisplay;
import me.gamercoder215.superadvancements.advancement.AFrame;
import net.minecraft.advancements.DisplayInfo;

import java.util.Arrays;

final class NMSDisplay1_20_R3 extends ADisplay {

    private final DisplayInfo handle;

    NMSDisplay1_20_R3(DisplayInfo handle) {
        this.handle = handle;

        this.setFrame(Arrays.stream(AFrame.values()).filter(f -> f.name().equalsIgnoreCase(handle.getType().getSerializedName())).findFirst().orElse(AFrame.TASK));
        this.setIcon(Wrapper1_20_R3.fromNMS(handle.getIcon()));
        this.setX(handle.getX());
        this.setY(handle.getY());

        if (handle.getBackground() != null) this.setBackgroundTexture(handle.getBackground().toString());
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
