# 🪙 SuperAdvancements
> Advanced &amp; Customizable Advancement API, made for SpigotMC and PaperMC 1.12+.

## Background
<details>
    <summary>Click to Expand</summary>

Advancements are one of the core features of Minecraft, and have been widely sought-out to modify and develop with. With a lack of understanding and documentation, it can be difficult to create custom advancements. 

This API serves to solve this by creating a simple and easy-to-use API that supports all versions of Minecraft where they exist, 1.12.2 and above. Featuring Bukkit, Spigot, and Paper support, this API is designed to be as flexible as possible, allowing for easy developing.
</details>

---

## ❓ Why?
- **Small**: The API aims to be as small, compact, and useful as possible, while also maintaining quality-of-life features.
- **Simple**: SuperAdvancements aims to have as much documentation and minimalization as possible in order to make usage seamless and comfortable.
- **Flexible**: SuperAdvancements is designed to be as flexible as possible, allowing for easy customization and modification. We often have API methods that directly access the methods we are using, allowing for even more modification.
- **Transparent**: SuperAdvancements is completely open source, has multiple issue reporting and support channels, and downloadable from your favorite repositories to depend and modify with.

---

## 📥 Installation
[![GitHub License](https://img.shields.io/github/license/GamerCoder215/SuperAdvancements)](https://github.com/GamerCoder215/SuperAdvancements/blob/master/LICENSE)
[![Project Status](https://github.com/GamerCoder215/SuperAdvancements/actions/workflows/build.yml/badge.svg)](https://github.com/GamerCoder215/SuperAdvancements/actions/workflows/build.yml)
[![](https://jitpack.io/v/GamerCoder215/SuperAdvancements.svg)](https://jitpack.io/#GamerCoder215/SuperAdvancements)
[![](https://jitci.com/gh/GamerCoder215/SuperAdvancements/svg)](https://jitci.com/gh/GamerCoder215/SuperAdvancements)
![GitHub release (latest by date)](https://img.shields.io/github/v/release/GamerCoder215/SuperAdvancements?style=plastic)

<details>
    <summary>Maven</summary>

```xml
<project>
    
    <!-- Import CodeMC Repo -->
    
    <repositories>
        <repository>
            <id>codemc-snapshots</id>
            <url>https://repo.codemc.io/repository/maven-releases/</url>
        </repository>
    </repositories>
    
    <dependencies>
        <!-- Replace [VERSION] with your Project Version -->

        <!-- 'superadvancements-spigot' for developing with Strings and BaseComponents -->
        <dependency>
            <groupId>me.gamercoder215.superadvancements</groupId>
            <artifactId>superadvancements-spigot</artifactId>
            <version>[VERSION]</version>
        </dependency>

        <!-- 'superadvancements-paper' for developing with Adventure Components -->
        <dependency>
            <groupId>me.gamercoder215.superadvancements</groupId>
            <artifactId>superadvancements-paper</artifactId>
            <version>[VERSION]</version>
        </dependency>
    </dependencies>
    
</project>
```
</details>

<details>
    <summary>Gradle (Groovy)</summary>

```gradle
repositories {
    maven { url 'https://repo.codemc.io/repository/maven-releases/' }
}

dependencies {
    // Replace [VERSION] with your Project Version

    // 'superadvancements-spigot' for developing with Strings and BaseComponents
    implementation 'me.gamercoder215.superadvancements:superadvancements-spigot:[VERSION]'

    // 'superadvancements-paper' for developing with Adventure Components
    implementation 'me.gamercoder215.superadvancements:superadvancements-paper:[VERSION]'
}
```
</details>

<details>
    <summary>Gradle (Kotlin DSL)</summary>

```kotlin
repositories {
    maven(url = "https://repo.codemc.io/repository/maven-releases/")
}

dependencies {
    // Replace [VERSION] with your Project Version

    // 'superadvancements-spigot' for developing with Strings and BaseComponents
    implementation("me.gamercoder215.superadvancements:superadvancements-spigot:[VERSION]")

    // 'superadvancements-paper' for developing with Adventure Components
    implementation("me.gamercoder215.superadvancements:superadvancements-paper:[VERSION]")
}
```
</details>