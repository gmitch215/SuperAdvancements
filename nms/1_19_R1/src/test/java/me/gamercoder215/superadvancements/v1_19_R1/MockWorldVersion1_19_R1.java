package me.gamercoder215.superadvancements.v1_19_R1;

import com.mojang.bridge.game.PackType;
import net.minecraft.SharedConstants;
import net.minecraft.WorldVersion;
import net.minecraft.world.level.storage.DataVersion;

import java.util.Date;
import java.util.UUID;

final class MockWorldVersion1_19_R1 implements WorldVersion {

    private final String id = UUID.nameUUIDFromBytes("test".getBytes()).toString().replace("-", "");
    private final Date build = new Date();

    public MockWorldVersion1_19_R1() {}

    @Override
    public Date getBuildTime() {
        return build;
    }

    @Override
    public DataVersion getDataVersion() {
        return new DataVersion(3120, "test");
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public String getName() {
        return "test";
    }

    @Override
    public String getReleaseTarget() {
        return "1.19.2";
    }

    @Override
    public int getPackVersion(PackType arg0) {
        return arg0 == PackType.DATA ? 10 : 9;
    }

    @Override
    public int getProtocolVersion() {
        return SharedConstants.getProtocolVersion();
    }

    @Override
    public boolean isStable() {
        return false;
    }

}
