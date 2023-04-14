use crate::NulStr;
use core::num::{NonZeroU16, NonZeroU32};
use uuid::Uuid;

#[repr(u64)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MediaType {
    Generic = 0,
    Optical = 1,
    Tftp = 2,
}

pub type UuidBytes = [u8; 16];

#[repr(C)]
#[derive(Debug)]
pub struct File {
    /// Revision of this structure.
    revision: u64,
    /// The address of the file.
    virtual_base: u64,
    /// The size of the file.
    length: u64,
    /// The path of the file within the volume, with a leading slash.
    path: NulStr,
    /// A command line associated with the file.
    cmdline: NulStr,
    /// Type of media file resides on.
    media_type: MediaType,
    unused: [u8; 4],
    /// If non-0, this is the IP of the TFTP server the file was loaded from.
    tftp_ip: u32,
    /// Likewise, but port.
    tftp_port: u32,
    /// 1-based partition index of the volume from which the file was loaded. If 0, it
    /// means invalid or unpartitioned.
    partition_index: u32,
    /// If non-0, this is the ID of the disk the file was loaded from as reported in its MBR.
    mbr_disk_id: u32,
    /// If non-0, this is the UUID of the disk the file was loaded from as reported in its GPT.
    gpt_disk_uuid: UuidBytes,
    /// If non-0, this is the UUID of the partition the file was loaded from as reported in the GPT.
    gpt_part_uuid: UuidBytes,
    /// If non-0, this is the UUID of the filesystem of the partition the file was loaded from.
    part_uuid: UuidBytes,
}

impl File {
    #[inline]
    pub const fn revision(&self) -> u64 {
        self.revision
    }

    #[inline]
    pub fn data(&self) -> &[u8] {
        unsafe {
            core::slice::from_raw_parts(
                usize::try_from(self.virtual_base).unwrap() as *mut u8,
                self.length.try_into().unwrap(),
            )
        }
    }

    #[inline]
    pub fn path(&self) -> &str {
        (&self.path).into()
    }

    #[inline]
    pub fn cmdline(&self) -> &str {
        (&self.cmdline).into()
    }

    #[inline]
    pub const fn media_type(&self) -> MediaType {
        self.media_type
    }

    #[inline]
    pub fn tftp_info(&self) -> (Option<NonZeroU32>, Option<NonZeroU16>) {
        (
            NonZeroU32::new(self.tftp_ip),
            NonZeroU16::new(self.tftp_port.try_into().unwrap()),
        )
    }

    #[inline]
    pub const fn partition_index(&self) -> Option<NonZeroU32> {
        NonZeroU32::new(self.partition_index)
    }

    #[inline]
    pub const fn mbr_disk_id(&self) -> Option<NonZeroU32> {
        NonZeroU32::new(self.mbr_disk_id)
    }

    #[inline]
    pub const fn gpt_disk_uuid(&self) -> Uuid {
        Uuid::from_bytes(self.gpt_disk_uuid)
    }

    #[inline]
    pub const fn gpt_part_uuid(&self) -> Uuid {
        Uuid::from_bytes(self.gpt_part_uuid)
    }

    #[inline]
    pub const fn partition_uuid(&self) -> Uuid {
        Uuid::from_bytes(self.part_uuid)
    }
}
