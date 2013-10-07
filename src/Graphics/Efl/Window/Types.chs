{-# Language ForeignFunctionInterface #-}
module Graphics.Efl.Window.Types (
   EngineType(..)
) where

#include <Ecore_Evas.h>

{#enum _Ecore_Evas_Engine_Type as EngineType {underscoreToCase} deriving (Eq,Show) #}
{#enum _Ecore_Evas_Avoid_Damage_Type as AvoidDamageType {underscoreToCase} deriving (Eq,Show) #}
{#enum _Ecore_Evas_Object_Associate_Flags as ObjectAssociateFlags {underscoreToCase} deriving (Eq,Show) #}
