package me.gamercoder215.superadvancements.v1_20_R3;

import net.minecraft.SharedConstants;
import net.minecraft.WorldVersion;
import net.minecraft.server.packs.PackType;
import net.minecraft.world.level.storage.DataVersion;

import java.util.Date;
import java.util.UUID;

final class MockWorldVersion1_20_R3 implements WorldVersion {

    private final String id = UUID.nameUUIDFromBytes("test".getBytes()).toString().replace("-", "");
    private final Date build = new Date();

    public MockWorldVersion1_20_R3() {}

    @Override
    public Date getBuildTime() {
        return build;
    }

    @Override
    public DataVersion getDataVersion() {
        return new DataVersion(3698, "test");
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
    public int getPackVersion(PackType arg0) {
        return 22;
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
