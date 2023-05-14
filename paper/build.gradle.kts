import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar

dependencies {
    api(project(":superadvancements"))
    api(project(":superadvancements-abstract"))
    api(project(":superadvancements-spigot"))

    compileOnly("com.destroystokyo.paper:paper-api:1.12.2-R0.1-SNAPSHOT") {
        version {
            strictly("1.12.2-R0.1-SNAPSHOT")
        }
    }
    compileOnly("net.kyori:adventure-api:4.13.0")

    testImplementation("com.destroystokyo.paper:paper-api:1.12.2-R0.1-SNAPSHOT") {
        version {
            strictly("1.12.2-R0.1-SNAPSHOT")
        }
    }
    testImplementation("net.kyori:adventure-api:4.13.0")

    listOf(
        "1_12_R1",
        "1_13_R1",
        "1_13_R2",
        "1_14_R1",
        "1_15_R1",
        "1_16_R1",
        "1_16_R2",
        "1_16_R3",
        "1_17_R1",
        "1_18_R1",
        "1_18_R2",
        "1_19_R1",
        "1_19_R2",
        "1_19_R3"
    ).forEach {
        api(project(":superadvancements-$it"))
    }
}

description = "Paper (Adventure Components) Implementation of the SuperAdvancements API"

java {
    withSourcesJar()
    withJavadocJar()
}

tasks {
    javadoc {
        sourceSets["main"].allJava.srcDir("src/main/javadoc")

        exclude("**/abstract/**", "**/nms/**")
    }

    withType<ShadowJar> {
        dependsOn("sourcesJar", "javadocJar")
    }
}